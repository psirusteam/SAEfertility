#################################################
#             Proyecto : SAEfertility           #
#       Direct estimation  bases - PERU         #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
# install.packages("DHS.rates")


library(dplyr)
library(survey)
library(srvyr)
library(tidyr)
library(writexl)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")


base_nacimientos <- readRDS(file.path(output, "base_nacimientos.rds"))
base_MEF <- readRDS(file.path(output, "base_MEF.rds"))

################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################

# --------------------------------------#
#                ASFR                   #
# --------------------------------------#

#Step 1. Calcular cuantos meses vivio cada mujer en los  diferentes grupos de edad
# Por cada mujer, se genera una fila por cada uno de los 36 meses previos a la entrevista

base_exp <- base_MEF %>%
  mutate(
    start_month = date_entrevista - 36, # Inicio del t: 36 meses antes
    end_month = date_entrevista - 1 # Fin del t: el mes anterior a la entrevista
  ) %>%
  rowwise() %>%
  mutate(meses = list(start_month:end_month)) %>% 
  unnest(cols = c(meses)) %>%
  ungroup() %>%
  mutate(
    edad_mensual = (meses - date_nac_muj) / 12,         # Se calcula la edad mensual de cada mujer en cada mes observado.
    grupo_edad = case_when(                             # Se clasifica en los grupos quinqueniales.
      edad_mensual >= 15 & edad_mensual < 20 ~ "15-19",
      edad_mensual >= 20 & edad_mensual < 25 ~ "20-24",
      edad_mensual >= 25 & edad_mensual < 30 ~ "25-29",
      edad_mensual >= 30 & edad_mensual < 35 ~ "30-34",
      edad_mensual >= 35 & edad_mensual < 40 ~ "35-39",
      edad_mensual >= 40 & edad_mensual < 45 ~ "40-44",
      edad_mensual >= 45 & edad_mensual < 50 ~ "45-49",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(grupo_edad)) %>%
  mutate(evento = 0, expo =1/12)

#37117*36=1336216


# Step 2. Calcular los nacimientos ocurridos en últimos 3 años
# Se une cada nacimiento con los datos de su madre.

base_nacimientos2 <- base_nacimientos %>%
  mutate(
    edad_madre_nac = (date_nac_hij - date_nac_muj) / 12, #Se calcula la edad de la madre al momento del parto
    grupo_edad = case_when(
      edad_madre_nac >= 15 & edad_madre_nac < 20 ~ "15-19",
      edad_madre_nac >= 20 & edad_madre_nac < 25 ~ "20-24",
      edad_madre_nac >= 25 & edad_madre_nac < 30 ~ "25-29",
      edad_madre_nac >= 30 & edad_madre_nac < 35 ~ "30-34",
      edad_madre_nac >= 35 & edad_madre_nac < 40 ~ "35-39",
      edad_madre_nac >= 40 & edad_madre_nac < 45 ~ "40-44",
      edad_madre_nac >= 45 & edad_madre_nac < 50 ~ "45-49",
      TRUE ~ NA_character_
    ),
    en_periodo = date_nac_hij >= (date_entrevista - 36) & date_nac_hij <= (date_entrevista - 1)
  ) %>%
  filter(!is.na(grupo_edad), en_periodo == TRUE) %>% #Se filtran solo los nacimientos ocurridos en los 36 meses antes de la entrevista y donde la madre tenía entre 15 y 49 años.
  mutate(evento = 1, expo = 1/12)

# Se seleccionan únicamente los nacimientos que ocurrieron durante los 36 meses 
# previos a la entrevista (en_periodo) y cuya madre tenía entre 15 y 49 años 
# al momento del parto (grupo_edad), cumpliendo así con los criterios estándar 
# para el cálculo de las tasas específicas de fecundidad (ASFR) según grupos 
# quinquenales de edad.


# Step 3. Unificación de base de base nacimientos(mujeres con evento de nacimiento (evento = 1)
# y base mujeres expandida (mujeres en riesgo (evento = 0)
base_completa <- bind_rows(base_nacimientos2 %>%
                           select(id_individual, fep_m, upm,dam, area,etnia,anoest, strata, grupo_edad, evento, expo),
                           base_exp)

base_completa$fep_m <- base_completa$fep_m/1000000
# Step 3. Cálculo de ASFR con survey
options(survey.lonely.psu="adjust")

disenio_svy <- svydesign(
  id = ~upm,
  strata = ~strata,
  weights = ~fep_m,
  data = base_completa,
  nest = TRUE
)

# total - edad
asfr <- svyby(formula = ~evento,denominator = ~expo,by = ~grupo_edad,design = disenio_svy,FUN = svyratio,vartype = c("cv","se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)

write_xlsx(asfr, path = file.path(output, "asfr/asfr_36meses.xlsx"))

#dam - edad
asfr_dam <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + grupo_edad,design = disenio_svy,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_dam, path = file.path(output, "asfr/asfr_36meses_dam.xlsx"))

#dam - edad - area
asfr_dam_area <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + area + grupo_edad,design = disenio_svy,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_dam_area, path = file.path(output, "asfr/asfr_36meses_dam_area.xlsx"))

#dam - edad - etnia
asfr_dam_etnia <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + etnia + grupo_edad,design = disenio_svy,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_dam_area, path = file.path(output, "asfr/asfr_36meses_dam_etnia.xlsx"))

#dam - edad - anoest
asfr_dam_anoest<- svyby(formula = ~evento,denominator = ~expo,by = ~dam + anoest + grupo_edad,design = disenio_svy,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_dam_area, path = file.path(output, "asfr/asfr_36meses_dam_anoest.xlsx"))

# -----------------------------------------------#
#   Tasa de fecundidad de las adolescentes 15_19 #
# -----------------------------------------------#

disenio_1519 <- subset(disenio_svy, grupo_edad == "15-19")

asfr_1519 <- svyby(
  formula = ~evento,
  denominator = ~expo,
  by = ~dam,
  design = disenio_1519,
  FUN = svyratio,
  vartype = c("cv", "se"),
  na.rm = TRUE
) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_1519, path = file.path(output, "asfr15-19/asfr1519_36meses_dam.xlsx"))

#dam - etnia
asfr_1519_etn <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + etnia,design = disenio_1519,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_1519_etn, path = file.path(output, "asfr15-19/asfr1519_36meses_dam_etnia.xlsx"))

#dam - area
asfr_1519_area <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + area,design = disenio_1519,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_1519_area, path = file.path(output, "asfr15-19/asfr1519_36meses_dam_area.xlsx"))

#dam - anoest
asfr_1519_anoest <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + anoest,design = disenio_1519,FUN = svyratio,vartype = c("cv", "se"),na.rm = TRUE) %>%
  mutate(asfr_mil = `evento/expo` * 1000)%>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(asfr_1519_anoest, path = file.path(output, "asfr15-19/asfr1519_36meses_dam_anoest.xlsx"))


# --------------------------------------#
#                TFR                    #
# --------------------------------------#

# Step 4. Cálculo de TFR

tfr <- sum(asfr$`evento/expo`, na.rm = TRUE) * 5
# Error estándar del TFR
se_tfr <- sqrt(sum((asfr$`se.evento/expo`)^2, na.rm = TRUE)) * 5
# Coeficiente de variación en porcentaje
cv_tfr <- (se_tfr / tfr) * 100

tfr <- data.frame(
  TFR = round(tfr, 2),
  se = round(se_tfr, 2),
  CV = round(cv_tfr, 2)
)

write_xlsx(tfr, path = file.path(output, "tfr/tfr_36meses.xlsx"))

TFR_por_dam <- asfr_dam %>%
  group_by(dam) %>%
  summarise(
    TFR = sum(`evento/expo`) * 5,
    se = sqrt(sum(`se.evento/expo`^2)) * 5,
    CV = (se / TFR) * 100
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(TFR_por_dam, path = file.path(output, "tfr/tfr_36meses_dam.xlsx"))

TFR_por_area <- asfr_dam_area %>%
  group_by(dam, area) %>%
  summarise(
    TFR = sum(`evento/expo`) * 5,
    se = sqrt(sum(`se.evento/expo`^2)) * 5,
    CV = (se / TFR) * 100
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(TFR_por_area, path = file.path(output, "tfr/tfr_36meses_dam_area.xlsx"))



TFR_por_etnia <- asfr_dam_etnia %>%
  group_by(dam, etnia) %>%
  summarise(
    TFR = sum(`evento/expo`) * 5,
    se = sqrt(sum(`se.evento/expo`^2)) * 5,
    CV = (se / TFR) * 100
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(TFR_por_etnia, path = file.path(output, "tfr/tfr_36meses_dam_etnia.xlsx"))



TFR_por_anoest <- asfr_dam_anoest %>%
  group_by(dam, anoest) %>%
  summarise(
    TFR = sum(`evento/expo`) * 5,
    se = sqrt(sum(`se.evento/expo`^2)) * 5,
    CV = (se / TFR) * 100
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_xlsx(TFR_por_anoest, path = file.path(output, "tfr/tfr_36meses_dam_anoest.xlsx"))


# --------------------------------------#
#   Promedio de hijos nacidos vivos     #
# --------------------------------------#

#Step 1. Agrupar la base de nacimiento por mujeres

base_nacxmuj <- base_nacimientos %>%
  group_by(id_individual) %>%
  summarise(hijos_nacidos = n())

#Step 2. Unir a la base de mujeres

base_MEF <- base_MEF %>%
  left_join(base_nacxmuj, by = "id_individual")  %>%
  mutate(hijos_nacidos = ifelse(is.na(hijos_nacidos), 0, hijos_nacidos))

base_MEF$fep_m <- base_MEF$fep_m/1000000

#Step 3. Definir el objeto svydesign
base_filter <- base_MEF %>% filter(edad_actual >= 15 & edad_actual <=49 )

disenio_svy2 <- svydesign(
  id = ~upm,
  strata = ~strata,
  weights = ~fep_m,
  data = base_filter,
  nest = TRUE
)

#Step 4. Calcular el promedio de nacidos vivos

options(survey.lonely.psu="adjust")

prom_hi <- svymean(~hijos_nacidos, disenio_svy2, vartype = c("se", "cv"), na.rm = TRUE)

pro_hij <- data.frame(
  media = coef(prom_hi),
  SE    = SE(prom_hi),
  CV    = cv(prom_hi)
)

write_xlsx(pro_hij, path = file.path(output, "prom_hij/promedio_hij_vivos.xlsx"))

#dam

prom_hi_dam <- svyby(~hijos_nacidos,by = ~dam,design = disenio_svy2,FUN = svymean,vartype = c("se", "cv"),na.rm = TRUE) %>%mutate(
    Promedio = round(hijos_nacidos, 2),
    SE = round(se, 2),
    CV = round(cv, 2)
  )

write_xlsx(prom_hi_dam %>% select(dam,Promedio, SE,CV),path = file.path(output, "prom_hij/promedio_hij_vivos_dam.xlsx"))

#dam - area

prom_hi_are <- svyby(~hijos_nacidos,by = ~dam+area,design = disenio_svy2,FUN = svymean,vartype = c("se", "cv"),na.rm = TRUE) %>%mutate(
  Promedio = round(hijos_nacidos, 2),
  SE = round(se, 2),
  CV = round(cv, 2)
)

write_xlsx(prom_hi_are %>% select(dam,area,Promedio, SE,CV),path = file.path(output, "prom_hij/promedio_hij_vivos_dam_area.xlsx"))


#dam - etnia

prom_hi_etn <- svyby(~hijos_nacidos,by = ~dam+etnia,design = disenio_svy2,FUN = svymean,vartype = c("se", "cv"),na.rm = TRUE) %>%mutate(
  Promedio = round(hijos_nacidos, 2),
  SE = round(se, 2),
  CV = round(cv, 2)
)

write_xlsx(prom_hi_etn %>% select(dam,etnia, Promedio, SE,CV),path = file.path(output, "prom_hij/promedio_hij_vivos_dam_etnia.xlsx"))


#dam - edad

prom_hi_eda <- svyby(~hijos_nacidos,by = ~dam+edad_grup,design = disenio_svy2,FUN = svymean,vartype = c("se", "cv"),na.rm = TRUE) %>%mutate(
  Promedio = round(hijos_nacidos, 2),
  SE = round(se, 2),
  CV = round(cv, 2)
)

write_xlsx(prom_hi_eda %>% select(dam,Promedio, SE,CV),path = file.path(output, "prom_hij/promedio_hij_vivos_dam_edad.xlsx"))




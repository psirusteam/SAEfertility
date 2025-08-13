#################################################
#             Proyecto : SAEfertility           #
#       Direct estimation  bases - PERU         #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(tidyr)
library(writexl)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_nacimientos <- readRDS("Modelo_area/PER_2024/ASFR/output/base_nacimientos.rds")
base_MEF <- readRDS("Modelo_area/PER_2024/ASFR/output/base_MEF.rds")

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


#dam - edad
asfr <- svyby(formula = ~evento,denominator = ~expo,by = ~dam + grupo_edad,design = disenio_svy,FUN = svyratio,deff = TRUE,na.rm = TRUE)

asfr <- asfr %>% rename(asfr = `evento/expo`, se = `se.evento/expo`, deff = DEff) %>% mutate(asfr_mil = asfr * 1000) %>% select(asfr,se,deff,asfr_mil)

nupm <- base_completa %>% distinct(dam,grupo_edad,upm) %>%
  group_by(dam, grupo_edad) %>% 
  tally()



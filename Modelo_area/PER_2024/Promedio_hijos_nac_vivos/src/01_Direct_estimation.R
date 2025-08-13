#################################################
#             Proyecto : SAEfertility           #
#       Direct estimation        - PERU         #
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

base_nacimientos <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/base_nacimientos.rds")
base_MEF <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/base_MEF.rds")

################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################

# --------------------------------------#
#   Promedio de hijos nacidos vivos     #
# --------------------------------------#

#Step 1. Agrupar la base de nacimiento por mujeres

base_nacxmuj <- base_nacimientos %>% filter(edad_actual >= 15 & edad_actual <=49) %>% 
  group_by(id_individual) %>%
  summarise(hijos_nacidos = n())

#Step 2. Unir a la base de mujeres

base_MEF <- base_MEF %>%
  left_join(base_nacxmuj, by = "id_individual")  %>%
  mutate(hijos_nacidos = ifelse(is.na(hijos_nacidos), 0, hijos_nacidos))

base_MEF$fep_m <- base_MEF$fep_m/1000000
sum(base_MEF$fep_m)
#Step 3. Definir el diseño
options(survey.lonely.psu = "adjust")

diseno <- base_MEF %>%
  as_survey_design(
    ids = upm,
    strata = strata,
    weights = fep_m,
    nest = TRUE
  )

summary(diseno)

diseno %>%
  summarise(
    mean_hijos = survey_mean(
      hijos_nacidos,
      vartype = c("se", "cv"),
      deff = TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


estimacion <- diseno %>%
  group_by(dam, dame) %>%
  summarise(
    hijos_nacidos = survey_mean(
      hijos_nacidos,
      vartype = c("se", "cv"),
      deff = TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

nupm <- base_MEF %>% distinct(dam,dame,upm) %>%
  group_by(dam,dame) %>% 
  tally()

nd <- base_MEF %>%
  group_by(dam, dame) %>%
  summarise(nd = n(), .groups = "drop")

estimacion <- estimacion %>% left_join(
  nupm,  by = c("dam", "dame")) %>% left_join(
  nd, by = c("dam", "dame")) %>% rename(p_deff = hijos_nacidos_deff)



#Estimacion directa al nivel de representatividad de la encuesta para benchmaring
estimacion_dam <- diseno %>%
  group_by(dam) %>%
  summarise(
    hijos_nacidos = survey_mean(
      hijos_nacidos,
      vartype = c("se", "cv", "ci"),
      level = 0.95,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

saveRDS(
  estimacion_dam,
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_dam.rds"
)


#CRITERIOS DE CALIDAD------------------------------------
##Excluyendo registros por falta de calidad
#base datos excluyendo los registros que cumplen criterios de calidad para 
#modelo sae

base_sae <- estimacion %>% data.frame()%>%
  filter(nd > 40, n >= 4) %>%
  transmute(
    dam = dam,              # Id para los departamento
    dame = dame,              #Id para los distritos
    nd = nd,                # Número de observaciones por dominios
    hijos_nacidos = hijos_nacidos,      # Estimación de la variable
    vardir = hijos_nacidos_se ^ 2,      # Estimación de la varianza directa 
    cv = hijos_nacidos_se/hijos_nacidos,                       
    deff_muni = p_deff        # Deff por dominio municipal
  )

saveRDS(base_sae, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_phv.rds")

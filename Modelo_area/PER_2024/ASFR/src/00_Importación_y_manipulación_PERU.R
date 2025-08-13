#################################################
#             Proyecto : SAEfertility           #
#       Lectura y procesamiento  bases - PERU   #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
# install.packages("DHS.rates")

library(DHS.rates)
library(dplyr)
library(survey)
library(srvyr)
library(ggplot2)
getwd()

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_MEF <- read.csv("Modelo_area/PER_2024/ASFR/input/REC0111_2024.csv")# Modulo mujeres en edad fertil 12 - 49 años
base_emb <- read.csv("Modelo_area/PER_2024/ASFR/input/REC21_2024.csv") #Historial de embarazos


# --------------------------------------#
#            Woman module               #
# --------------------------------------#

base_MEF <- base_MEF %>% select(
  id_individual = CASEID,                  # Identificación única del cuestionario individual
  id_hogar = HHID,                         # Identificación del hogar
  codigo_pais = V000,                      # Códigopaís 
  upm = V021,                     # Unidad de muestreo conglomerado
  num_vivienda = V002,                     # Número de vivienda dentro del conglomerado
  num_linea_entrevistada = V003,           # Número de línea de la mujer entrevistada
  date_entrevista = V008,                  # Fecha de la entrevista
  date_nac_muj = V011,                     # Fecha de nacimiento de la mujer
  edad_actual = V012,                      # Edad actual de la entrevistada
  edad_grup = V013,                        # Edad actual por grupos
  dam  = V101,                             # Departamento
  area = V025,                             # Area
  nivel_edu = V106,                        # Nivel educativo más alto
  anoest = V107,                           # Año/grado de educacion mas alto aprobado
  fep_m = V005 ,                           # Factpr total muj
  etnia = V131,                            # Etnia
  strata = V022
  
) %>% select(
  id_individual, id_hogar, codigo_pais, upm,
  num_vivienda, num_linea_entrevistada, date_entrevista, date_nac_muj,
  edad_actual, edad_grup, dam, area, nivel_edu, anoest, fep_m, etnia,strata
)
base_MEF %>% group_by(id_individual) %>% tally()
saveRDS(base_MEF, file ="Modelo_area/PER_2024/ASFR/output/base_MEF.rds")
# --------------------------------------#
#            Births module              #
# --------------------------------------#

base_emb <- base_emb %>% mutate(
  id_individual = CASEID,                  # Identificación única del cuestionario individual
  ord_nac = BORD ,                         # Numero orden nacimiento
  cant_hij_parto = B0,                     # Parto unico o multiple
  day_nac = BD,                            # Dia de nacimiento del hijo
  mont_nac = B1,                           # Mes nacimiento
  year_nac = B2,                           # Año nacimeinto
  date_nac_hij = B3,                       # Fecha de nacimiento, Codificación centenaria de meses (CMC)
  sex_hij = B4,                            # Sexo del niño
  vivo_hij = B5,                          # El niño está vivo
  edad_muerte = B6,                       # Edad a la muerte (en días, meses o años según codificación)
  edad_muerte_imp = B7,                  # Edad a la muerte en meses (imputada)
  edad_actual_hij = B8                   # Edad actual del niño (si está vivo)
  
) %>%  
  select(
    id_individual, ord_nac, cant_hij_parto, day_nac, mont_nac,
    year_nac, date_nac_hij, sex_hij, vivo_hij, edad_muerte,
    edad_muerte_imp, edad_actual_hij
  )

# Variables a heredar de base_MEF
vars_mujer <- c("date_entrevista", "date_nac_muj", "fep_m", "upm", "strata",
                "dam", "area", "codigo_pais", "nivel_edu",
                "id_hogar", "num_vivienda", "num_linea_entrevistada", 
                "edad_actual", "edad_grup", "anoest", "etnia", "id_individual")

# Hacer el left_join para agregar datos de la madre a cada nacimiento
base_emb <- base_emb %>%
  left_join(base_MEF %>% select(all_of(vars_mujer)), by = "id_individual")

base_emb %>% group_by(id_individual) %>% tally()


saveRDS(base_emb, file = "Modelo_area/PER_2024/ASFR/output/base_nacimientos.rds")

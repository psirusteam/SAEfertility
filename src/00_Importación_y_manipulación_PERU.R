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


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")



base_MEF <- read.csv(file.path(input, "REC0111_2024.csv"), stringsAsFactors = FALSE)# Modulo mujeres en edad fertil 12 - 49 años
base_emb <- read.csv(file.path(input, "REC21_2024.csv"), stringsAsFactors = FALSE) #Historial de embarazos


# --------------------------------------#
#            Woman module               #
# --------------------------------------#

base_MEF <- base_MEF %>% select(
  id_individual = CASEID,                  # Identificación única del cuestionario individual
  id_hogar = HHID,                         # Identificación del hogar
  codigo_pais = V000,                      # Códigopaís 
  conglomerado = V001,                     # Número del conglomerado (PSU)
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
  
) %>% select(
id_individual, id_hogar, codigo_pais, conglomerado,
    num_vivienda, num_linea_entrevistada, date_entrevista, date_nac_muj,
    edad_actual, edad_grup, dam, area, nivel_edu, anoest, fep_m, etnia
  )
base_MEF %>% group_by(id_individual) %>% tally()
saveRDS(base_MEF, file = file.path(output, "base_MEF.rds"))
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
  
) %>%  
  select(
  id_individual,ord_nac,cant_hij_parto,day_nac,
  mont_nac, year_nac, date_nac_hij, sex_hij)

base_emb %>% group_by(id_individual) %>% tally()


saveRDS(base_emb, file = file.path(output, "base_nacimientos.rds"))

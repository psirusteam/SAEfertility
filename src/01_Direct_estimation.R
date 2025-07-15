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

library(DHS.rates)
library(dplyr)
library(survey)
library(srvyr)
library(ggplot2)
library(tidyr)


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

#Step 1. Calcular cuantos meses vivio cada mujer en los 7 diferentes grupos de edad

# Crear una fila por mes en los 36 meses antes de la entrevista

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
    edad_mensual = (meses - date_nac_muj) / 12,
    grupo_edad = case_when(
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
  filter(!is.na(grupo_edad))

#37117*36=1336216

#Step 2. Calcular el denominador

denominador <- base_exp %>%
  group_by(grupo_edad) %>%
  summarise(mujer_meses_pond = sum((fep_m/1000000), na.rm = TRUE)) %>%
  mutate(mujer_anios_pond = mujer_meses_pond / 12)

# Step 3. Numerador: Calcular los nacimientos ocurridos en últimos 3 años

base_nacimientos2 <- base_nacimientos %>%
  left_join(base_MEF %>% select(id_individual, date_entrevista, date_nac_muj, fep_m), by = "id_individual") %>%
  mutate(
    edad_madre_nac = (date_nac_hij - date_nac_muj) / 12,
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
  filter(!is.na(grupo_edad), en_periodo == TRUE)

# Se seleccionan únicamente los nacimientos que ocurrieron durante los 36 meses 
# previos a la entrevista (en_periodo) y cuya madre tenía entre 15 y 49 años 
# al momento del parto (grupo_edad), cumpliendo así con los criterios estándar 
# para el cálculo de las tasas específicas de fecundidad (ASFR) según grupos 
# quinquenales de edad.

numerador <- base_nacimientos2 %>%
  group_by(grupo_edad) %>%
  summarise(nacimientos_pond = sum((fep_m/1000000), na.rm = TRUE))

# Step 4. Cálculo de ASFR

asfr <- left_join(numerador, denominador, by = "grupo_edad") %>%
  mutate(asfr = nacimientos_pond / mujer_anios_pond,
         asfr_mil = asfr * 1000)

# --------------------------------------#
#                TFR                    #
# --------------------------------------#

# Step 5. Cálculo de TFR

tfr <- sum(asfr$asfr, na.rm = TRUE) * 5;tfr

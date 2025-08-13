#################################################
#             Proyecto : SAEfertility           #
#       Direct estimation Mortality   DHS       #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
# install.packages("DHS.rates")
library(DHS.rates)
library(dplyr)
library(writexl)
library(tidyr)
library(matrixStats)

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


# -------------------------------
# Preparar base TOTAL
# -------------------------------


# Unir la base de mujeres
base_total <-left_join(base_emb, base_MEF %>% select("V008","V021", "V022", "V005", "CASEID" ),by = "CASEID") 

# Convertir nombres a minúsculas para estandarizar
names(base_total) <- tolower(names(base_total))


# -------------------------------
#  Mortality indicators
# -------------------------------


(chmort(base_total))

# Mostrar el código de la función
chmort
CHMORT5

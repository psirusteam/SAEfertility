#################################################
#             Proyecto : SAEfertility           #
#       Lectura y manipulación  bases - PERU    #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
install.packages("DHS.rates")

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

rech1_2024 <- read.csv(file.path(input, "RECH1_2024.csv"), stringsAsFactors = FALSE)
rec41_2024 <- read.csv(file.path(input, "REC41_2024.csv"), stringsAsFactors = FALSE)
rec94_2024 <- read.csv(file.path(input, "REC94_2024.csv"), stringsAsFactors = FALSE)
rech0_2024 <- read.csv(file.path(input, "RECH0_2024.csv"), stringsAsFactors = FALSE)


str(rech1_2024)
str(rec41_2024)
str(rec94_2024)
str(rech0_2024)

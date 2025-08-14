#################################################
#             Proyecto : SAEfertility           #
#                   Redatam                     #
#################################################

rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(purrr)
library(dplyr)
library(Rcpp)
library(RcppProgress)
library(redatam)

peru <- redatam.open("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/input/cpv-per-2017-cde_diccionario.dicx")

redatam.entities(peru)
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "PERSONA")
redatam.variables(peru, "HOGAR")

CONTEOS <- redatam.query(peru, "freq DISTRITO.REDCODE
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02",
                         tot.omit = FALSE)

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter(if_all(matches("label"), ~ .x != "__tot__" & .x != "na" & .x != "mv_" ))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

muj_15_49 <- CONTEOS2 %>%
  transmute(dame = str_pad(
    string = REDCODEN1_value,
    width = 6,
    pad = "0"
  ),
  dam = str_sub(dame, start = 1,end = 2),    
  sexo = as.character(C5P023_value),
  edad = C5P0412_value,
  value)%>%
  filter(edad >= 15 & edad <= 49 & sexo == 2) %>% group_by(dam,dame)%>%
  summarise(n = sum(value), .groups = "drop")
  

# Suma del total mujeres de 15 a 49 años
sum(muj_15_49$n)


saveRDS(muj_15_49, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/censo_muj_15_49.rds")


## Promedio hijos nacidos vivos por dame ------------------------------------------

CONTEOS <- redatam.query(peru, "freq DISTRITO.REDCODE
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02
                                  by PERSONA.C5P27",
                         tot.omit = FALSE)

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter(if_all(matches("label"), ~ .x != "__tot__" & .x != "__na__" & .x != "__mv__" ))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

hijos_nacidos <- CONTEOS2 %>%
  transmute(dame = str_pad(
    string = REDCODEN1_value,
    width = 6,
    pad = "0"
  ),
  dam = str_sub(dame, start = 1,end = 2),   
  sexo = C5P023_value,
  edad = C5P0412_value,
  hijos_nacidos = C5P274_value,
  value)%>%
  filter(edad >= 15 & edad <= 49 & sexo == 2) %>% group_by(dam,dame)%>%
  summarise(hijos_nacidos = sum(value*hijos_nacidos), .groups = "drop")

hijos_nacidos <- hijos_nacidos %>% left_join(muj_15_49, by = c("dam","dame")) %>% 
  mutate(hijos_nacidos_2017 = hijos_nacidos / n) %>% dplyr::select(-c("hijos_nacidos", "n"))




saveRDS(
  hijos_nacidos,
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/censo_hijos_nacidos.rds"
)



#################################################
#             Proyecto : SAEfertility           #
#                   Benchmarking                #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(haven)
library(foreign)
library(tidyverse)
library(magrittr)
library(labelled)
library(survey)
library(srvyr)
library(gtsummary)
library(epiDisplay)
library(readxl)
library(knitr)
library(kableExtra)
library(rstan)
library(patchwork)  
library(magrittr)
library(tmap)
library(sf)
library(sp)
library(rstan)
library(patchwork)
library(magrittr)
library(rstan)
library(bayesplot)
library(posterior)
library(patchwork)
library(fastDummies)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

FH_estimacion <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimacion_promedio_hij_vivos_FH.rds")

censo <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/censo_muj_15_49.rds")

Direct_estimation_dam <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_dam.rds") %>%
  transmute(dam = str_pad(
    string = dam,
    width = 2,
    pad = "0"
  ),
  theta_dir_dam =hijos_nacidos,
  se_dam = hijos_nacidos_se,
  low_dam = hijos_nacidos_low,
  upp_dam = hijos_nacidos_upp)

Direct_estimation_dam$dam <- as.character(Direct_estimation_dam$dam)

################################################################################
###------------------------------- Benchmarking  ----------------------------###
################################################################################

FH_estimacion <- FH_estimacion %>% left_join(censo, by ="dame") 

N_dam_pp <- FH_estimacion %>% group_by(dam) %>%  
  summarise(dam_pp = sum(n))


##Realizar el consolidado de informacion obtenida de: 
##prediccion + estimacion directa
temp <- FH_estimacion %>%
  inner_join(N_dam_pp) %>% 
  inner_join(Direct_estimation_dam, by = "dam") %>% arrange (dame)

##Con la informacion organizada, realizar el calculo de los pesos 
##para el Benchmark
R_dam2 <- temp %>% group_by(dam) %>% 
  summarise(
    R_dam_RB = unique(theta_dir_dam) / sum((n  / dam_pp) * theta_pred)
  ) %>%
  left_join(Direct_estimation_dam) 

#Rdam es el ponderador. Este ponderador tiene que revisar y tiene que
#estar alrededor de 1.0. Si este valor esta muy alejado de 1, creo 
#que debemos pensar en un mejor modelo.

##Calculando los pesos para cada dominio----
pesos <- temp %>% 
  mutate(W_i = n / dam_pp) %>% 
  dplyr :: select(dame, W_i)


##Realizar la estimacion FH Benchmark----
estimacionesBench <- FH_estimacion %>%
  left_join(R_dam2, by = c("dam")) %>%
  mutate(theta_pred_RBench = R_dam_RB * theta_pred) %>%
  left_join(pesos) %>% 
  dplyr::select(dam, 
         dame, 
         W_i, 
         theta_pred, 
         theta_pred_RBench,Cv_theta_pred) %>% arrange (dame) 

##Validacion: estimacion FH-normal con Benchmark--------------------------------

temp<-estimacionesBench %>% group_by(dam) %>%
  summarise(theta_reg_RB = sum(W_i * theta_pred_RBench),
            theta_pred = sum(W_i * theta_pred)) %>%
  left_join(Direct_estimation_dam, by = "dam") %>% 
  arrange (dam) %>%
  mutate(id = 1:n())

temp %<>% gather(key = "Metodo", value = "Estimacion",
                 -id, -dam,-se_dam, -low_dam, -upp_dam)

a <- ggplot(data = temp, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_jitter(aes(color = Metodo), size = 2) +
  theme_bw(10) + 
  geom_line(aes(y = low_dam), linetype  = 2) +
  geom_line(aes(y = upp_dam),  linetype  = 2) +
  scale_x_continuous(breaks = temp$id,
                     labels =  temp$dam) +
  labs(y = "", x = "")

a

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_benchmarking.jpeg",
       scale = 3)

saveRDS(estimacionesBench, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimacion_promedio_hij_vivos_FH_bench.rds")

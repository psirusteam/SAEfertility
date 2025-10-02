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
  "Modelo_area/PER_2024/IMR/output/estimacion_IMR_FH_binomial_30092025.rds") 

FH_cadena <- readRDS(
  "Modelo_area/PER_2024/IMR/output/Cadenas_thetapred_30092025.rds") 

FH_cadena <- t(FH_cadena) 
colnames(FH_cadena) <- paste0("iter_", seq_len(ncol(FH_cadena)))
FH_cadena <- FH_cadena %>% as.data.frame()


FH_cadena <- cbind(FH_cadena,FH_estimacion %>% dplyr::select(provi))
options(scipen = 999) 

#Excluimos las estimaciones que superan el umbral de la estimación directa
#IMR - 0.14561

FH_estimacion <- FH_estimacion %>% filter(! theta_pred > 0.14561) 

censo <- readRDS(
  "Modelo_area/PER_2024/IMR/output/censo_hijos_nacidos.rds")

Direct_estimation_dam <- readRDS(
  "Modelo_area/PER_2024/IMR/output/estimación_directa_dam.rds") %>%
  transmute(dam = str_pad(
    string = dam,
    width = 2,
    pad = "0"),
    IMR_dam = (R/1000),
    se_dam =SE,
    low_dam = LCI,
    upp_dam = UCI
    
  )


################################################################################
###------------------------------- Benchmarking  ----------------------------###
################################################################################

FH_estimacion <- FH_estimacion %>% left_join(censo, by ="provi") 

N_dam_pp <- FH_estimacion %>% group_by(dam) %>%  
  summarise(dam_pp = sum(hijos_nacidos))


##Realizar el consolidado de informacion obtenida de: 
##prediccion + estimacion directa
temp <- FH_estimacion %>%
  inner_join(N_dam_pp) %>% 
  inner_join(Direct_estimation_dam, by = "dam") %>% arrange (provi)

##Con la informacion organizada, realizar el calculo de los pesos 
##para el Benchmark
R_dam2 <- temp %>% group_by(dam) %>% 
  summarise(
    R_dam_RB = unique(IMR_dam) / sum((hijos_nacidos  / dam_pp) * theta_pred)
  ) %>%
  left_join(Direct_estimation_dam) 


##Realizar el consolidado de informacion obtenida de: 
##cadenas de la pre + estimacion directa
temp_draws <- FH_cadena %>% inner_join(temp %>% dplyr::select(provi, IMR_dam, hijos_nacidos, dam_pp, dam),
                                       by = "provi")

##Con la informacion organizada, realizar el calculo de los pesos 
##para el Benchmark
R_dam2_draws <- temp_draws %>% 
  group_by(dam) %>%
  summarise(
    # Usamos across() para aplicar la misma fórmula a las 2000 columnas 'iter'
    across(.cols = starts_with("iter_"), 
           .names = "R_dam_RB_{.col}",
           .fns = ~ unique(IMR_dam) / sum((hijos_nacidos / dam_pp) * .x)
    )
  ) %>% 
  mutate(
    across(.cols = starts_with("R_dam_RB_"),
           .fns = ~ case_when(
             # Comprobación de NaN
             is.nan(.x) ~ 1,
             # Comprobación de Infinito (tanto positivo como negativo)
             is.infinite(.x) ~ 1,
             # Si es válido, mantiene el valor
             TRUE ~ .x 
           )
    )
  )



#Rdam es el ponderador. Este ponderador tiene que revisar y tiene que
#estar alrededor de 1.0. Si este valor esta muy alejado de 1, creo 
#que debemos pensar en un mejor modelo.

##Calculando los pesos para cada dominio----
pesos <- temp %>% 
  mutate(W_i = hijos_nacidos / dam_pp) %>% 
  dplyr :: select(provi, W_i)


##Realizar la estimacion FH Benchmark----
estimacionesBench <- FH_estimacion %>%
  left_join(R_dam2, by = c("dam")) %>%
  mutate(theta_pred_RBench = R_dam_RB * theta_pred) %>%
  left_join(pesos) %>% 
  dplyr::select(dam, 
                provi, 
                W_i, 
                theta_pred, 
                theta_pred_RBench,Cv_pred) %>% arrange (provi) 


estimacionesBench_draws <- temp_draws %>% 
  left_join(R_dam2_draws , by = c("dam")) %>% 
  mutate(
    # Iteramos sobre las columnas originales de simulaciones ("iter_1", "iter_2", etc.)
    across(.cols = starts_with("iter_"),
           .names = "{.col}_bench",
           .fns = ~ {
             col_name <- cur_column()
             rb_col_name <- paste0("R_dam_RB_", col_name)
             .x * get(rb_col_name)
           }
    )
  )%>%
  left_join(pesos)

saveRDS(estimacionesBench_draws,"Modelo_area/PER_2024/IMR/output/estimacion_IMR_bench_binomial_draws_30092025.rds" )

##Validacion: estimacion FH-normal con Benchmark--------------------------------

temp<-estimacionesBench %>% group_by(dam) %>%
  summarise(theta_reg_RB = sum(W_i * theta_pred_RBench),
            theta_pred = sum(W_i * theta_pred)) %>%
  left_join(Direct_estimation_dam, by = "dam") %>% 
  arrange (dam) %>%
  mutate(id = 1:n())

temp_draws <- estimacionesBench_draws %>% 
  transmute(
    theta_reg_RB_draws = rowMeans(
      dplyr::select(., starts_with("iter_") & ends_with("_bench"))
    ),
    
    theta_pred_draws = rowMeans(
      dplyr::select(., starts_with("iter_") & !ends_with("_bench"))
    ),
    dam,
    W_i
  ) %>%  group_by(dam) %>%
  summarise(
    theta_reg_RB_draws = sum(W_i * theta_reg_RB_draws),
    theta_pred_draws   = sum(W_i * theta_pred_draws)
  )

consolidado_temp <- left_join(temp, temp_draws, by = "dam")
consolidado_temp <- consolidado_temp %>% 
  mutate(theta_reg_RB = (theta_reg_RB) * 1000,
         theta_pred =  (theta_pred) *1000 ,
         theta_reg_RB_draws = (theta_reg_RB_draws) * 1000,
         theta_pred_draws =  (theta_pred_draws) *1000 ,
         IMR_dam = (IMR_dam) *1000) 

# temp %<>% gather(key = "Metodo", value = "Estimacion",
#                  -id, -dam,-se_dam, -low_dam, -upp_dam)

consolidado_temp %<>% gather(key = "Metodo", value = "Estimacion",
                             -id, -dam,-se_dam, -low_dam, -upp_dam)

# temp %<>% gather(key = "Metodo", value = "Estimacion",
#                  -id, -dam,-se_dam)
a <- ggplot(data = consolidado_temp, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_jitter(aes(color = Metodo), size = 2) +
  theme_bw(10) + 
  geom_line(aes(y = low_dam), linetype  = 2) +
  geom_line(aes(y = upp_dam),  linetype  = 2) +
  scale_x_continuous(breaks = temp$id,
                     labels =  temp$dam) +
  labs(y = "", x = "")

a

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/IMR/output/modelo_binomial_benchmarking_30092025.jpeg",
       scale = 3)

estimacionesBench <- estimacionesBench %>% mutate(
  theta_pred = theta_pred*1000,
  theta_pred_RBench = theta_pred_RBench*1000
  
) 

IMR_final <- estimacionesBench_draws %>% 
  transmute(
    dam,
    provi,
    IMR_cadenas = dplyr::select(., starts_with("iter_") & ends_with("_bench")),
    IMR_media = rowMeans(IMR_cadenas),
    IMR_sd = apply(IMR_cadenas, 1, sd)
  ) %>%
  transmute(
    dam,
    provi,
    IMR_media = IMR_media*1000,
    IMR_se = IMR_sd*1000,
    IMR_cv = IMR_sd / IMR_media*100,
    IMR_LCI = apply(IMR_cadenas, 1, quantile, probs = 0.025)*1000, 
    IMR_UCI = apply(IMR_cadenas, 1, quantile, probs = 0.975) *1000 
  ) 

saveRDS(
  IMR_final,
  "Modelo_area/PER_2024/IMR/output/estimacion_IMR_bench_binomial_30092025.rds"
)

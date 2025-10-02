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
library(stringr)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

IMR_cadena <- readRDS(
  "Modelo_area/PER_2024/IMR/output/estimacion_IMR_bench_binomial_draws_28092025.rds") 

CMR_cadena  <- readRDS(
  "Modelo_area/PER_2024/CMR/output/estimacion_CMR_bench_binomial_draws_28092025.rds") 

U5MR_cadenas <- IMR_cadena %>% 
  inner_join(CMR_cadena, by = c("dam","provi"), 
             suffix = c(".IMR", ".CMR")) %>% 
  transmute(
    provi, 
    across(.cols = starts_with("iter_") & ends_with("_bench.IMR"),
           .names = "U5MR_{gsub('.IMR', '', {.col}, fixed = TRUE)}",
           .fns = ~ {
             imr_col_name <- cur_column()
             cmr_col_name <- gsub(".IMR", ".CMR", imr_col_name, fixed = TRUE)
             1 - (1 - .x) * (1 - get(cmr_col_name))
           }
    )
  )


Direct_estimation_dam <- readRDS(
  "Modelo_area/PER_2024/U5MR/output/estimación_directa_dam.rds") %>%
  transmute(dam = str_pad(
    string = dam,
    width = 2,
    pad = "0"),
    U5MR_dam = R/1000,
    se_dam =SE,
    low_dam = LCI,
    upp_dam = UCI
  )

censo <- readRDS(
  "Modelo_area/PER_2024/IMR/output/censo_hijos_nacidos.rds")

################################################################################
###------------------------------- Benchmarking  ----------------------------###
################################################################################

# U5MR <- IMR_cadena %>% inner_join(CMR_cadena, by ="provi") %>% transmute(
#   U5MR_bench = (1-(1-IMR_bench)*(1-CMR_bench)),
#   U5MR = (1-(1-IMR)*(1-CMR)),
#   provi
# ) %>%  left_join(censo, by ="provi") 



N_dam_pp <- U5MR_cadenas %>% left_join(censo, by ="provi" ) %>% group_by(dam) %>%  
  summarise(dam_pp = sum(hijos_nacidos))

U5MR_cadenas <- U5MR_cadenas %>% left_join(censo %>% 
                                             dplyr::select(dam, provi, hijos_nacidos), by = "provi")

##Realizar el consolidado de informacion obtenida de: 
##prediccion + estimacion directa
temp <- U5MR_cadenas %>%
  inner_join(N_dam_pp) %>% 
  inner_join(Direct_estimation_dam, by = "dam") %>% arrange (provi)

##Calculando los pesos para cada dominio----
pesos <- temp %>% 
  mutate(W_i = hijos_nacidos / dam_pp) %>% 
  dplyr :: select(provi, W_i)


##Validacion: estimacion FH-normal con Benchmark--------------------------------

temp_draws <- temp %>% left_join(pesos, by ="provi") %>% 
  transmute(
    U5MR_bench = rowMeans(
      dplyr::select(., starts_with("U5MR_") & ends_with("_bench"))
    ),
    dam,
    W_i
  ) %>%  group_by(dam) %>%
  summarise(
    U5MR_bench = sum(W_i * U5MR_bench),
  )  %>%  left_join(Direct_estimation_dam, by = "dam") %>%
  arrange (dam) %>%
  mutate(id = 1:n())

# # temp<- temp %>% left_join(pesos, by = "provi") %>% group_by(dam) %>%
# #   summarise(U5MR = sum(W_i * U5MR),
# #             U5MR_bench = sum(W_i * U5MR_bench)) %>%
#   left_join(Direct_estimation_dam, by = "dam") %>%
#   arrange (dam) %>%
#   mutate(id = 1:n())

temp_1000 <- temp_draws %>% 
  mutate(U5MR_bench = (U5MR_bench) * 1000,
         U5MR_dam  = (U5MR_dam ) *1000) 

# temp2_100 <- readRDS("Modelo_area/PER_2024/IMR/output/TEMPORAL_u5mrbruta.rds" ) %>% transmute(
#   U5MR_bruta = U5MR,
#   dam
# )
# 
# temp_1000 <- left_join(temp_1000,temp2_100, by = "dam")
temp_1000 %<>% gather(key = "Metodo", value = "Estimacion",
                      -id, -dam,-se_dam, -low_dam, -upp_dam)




a <- ggplot(data = temp_1000, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_jitter(aes(color = Metodo), size = 2) +
  theme_bw(10) + 
  geom_line(aes(y = low_dam), linetype  = 2) +
  geom_line(aes(y = upp_dam),  linetype  = 2) +
  scale_x_continuous(breaks = temp_1000$id,
                     labels =  temp_1000$dam) +
  labs(y = "", x = "")

a

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/U5MR/output/modelo_FH_normal_benchmarking_28092025.jpeg",
       scale = 3)

U5MR_final <- temp %>% 
  transmute(
    dam,
    provi,
    U5MR_cadenas = dplyr::select(., starts_with("U5MR_") & ends_with("_bench")),
    U5MR_media = rowMeans(U5MR_cadenas),
    U5MR_sd = apply(U5MR_cadenas, 1, sd)
  ) %>%
  transmute(
    dam,
    provi,
    U5MR_media = U5MR_media*1000,
    U5MR_se = U5MR_sd*1000,
    U5MR_cv = U5MR_sd / U5MR_media*100,
    U5MR_LCI = apply(U5MR_cadenas, 1, quantile, probs = 0.025)*1000, 
    U5MR_UCI = apply(U5MR_cadenas, 1, quantile, probs = 0.975) *1000 
  ) 
saveRDS(
  U5MR_final,
  "Modelo_area/PER_2024/U5MR/output/estimacion_U5MR_FH_bench_28092025.rds"
)

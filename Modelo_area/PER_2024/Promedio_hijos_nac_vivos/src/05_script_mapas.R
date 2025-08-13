#################################################
#             Proyecto : SAEfertility           #
#                     Mapas                     #
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
library(dplyr)
library(fastDummies)
library(ggplot2)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################


ShapeSAE<-read_sf("Modelo_area/PER_2024/shape/DISTRITOS.shp")

FH_estimacion_bench <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimacion_promedio_hij_vivos_FH_bench.rds"
)

cols_to_avg <- c( "theta_pred", "Cv_theta_pred","theta_pred_RBench" )

# Calcular promedio para 120699 a partir de 120606 y 120604
# El shape tiene un solo registro para "120699" (MAZAMARI - PANGOA unidos),
# pero la tabla FH_estimacion_bench tiene 120606 (MAZAMARI) y 120604 (PANGOA) por separado.
# --- SOLUCIÓN ---
# Calcular el promedio de los distritos para 120606 y 120604
# y luego rellenar los NA de la fila 120699 con ese promedio.

prom_120699 <- FH_estimacion_bench %>%
  filter(dame %in% c("120606", "120604")) %>%
  summarise(across(all_of(cols_to_avg), ~mean(.x, na.rm = TRUE))) %>%
  mutate(IDDIST = "120699")



################################################################################
###------------------------------- Mapas  -----------------------------------###
################################################################################

##Unir información del shape a la base------------------------------------------


data <- ShapeSAE %>% left_join(FH_estimacion_bench, by = c("IDDIST" = "dame")) %>% 
  left_join(prom_120699, by = "IDDIST", suffix = c("", "_avg")) %>%
  mutate(
    across(
      all_of(cols_to_avg),
      ~ ifelse(IDDIST == "120699" & (is.na(.x) | !is.finite(.x)),
               get(paste0(cur_column(), "_avg")),
               .x)
    )
  )
  
brks <- c(-Inf, 1, 2, 3, Inf)

mapa <- tm_shape(data) +
  tm_polygons(
    col = "theta_pred_RBench",
    title = "Pro_hij_n_v",
    palette = "YlOrRd",
    style = "fixed",
    breaks = brks,
    colorNA = "grey90",
    lwd = 0.2, border.col = "grey"
  )

tmap_save(
  tmap_arrange(mapa),
  filename = file.path("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_benchmarking_mapa.jpeg"),
  width = 3000, height = 1500, dpi = 300
)

brks_cv <- c (0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6 )


mapa_cv <- tm_shape(data) +
  tm_polygons(
    col = "Cv_theta_pred",
    title = "CV",
    palette = "YlOrRd",
    style = "fixed",
    breaks = brks_cv,
    colorNA = "grey90",
    lwd = 0.2, border.col = "grey"
  )

tmap_save(
  tmap_arrange(mapa_cv),
  filename = file.path("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_benchmarking_CV_mapa.jpeg"),
  width = 3000, height = 1500, dpi = 300
)



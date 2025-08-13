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


FH_estimacion_bench <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimacion_promedio_hij_vivos_FH_bench.rds"
)

################################################################################
###------------------------------- Mapas  -----------------------------------###
################################################################################

##Leer shapefile de pais - Peru ------------------------------------------------

ShapeSAE<-read_sf("Modelo_area/PER_2024/shape/DISTRITOS.shp")

##Unir información del shape a la base------------------------------------------

data <- ShapeSAE %>% left_join(FH_estimacion_bench, by = c("IDDIST" = "dame")
  ) #solo 1873 distritos?


x <- data$theta_pred_RBench
n_clases <- 6

minx <- floor(min(x, na.rm = TRUE) * 10) / 10   
maxx <- ceiling(max(x, na.rm = TRUE) * 10) / 10
brks  <- seq(minx, maxx, length.out = n_clases + 1)

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

brks_cv <- c (0,0.2, 0.3, 0.4,  0.6,0.8 ,1.0, 3.0)


mapa_cv <- tm_shape(data) +
  tm_polygons(
    col = "Cv_theta_pred",
    title = "CV_prom_hij_n_v",
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



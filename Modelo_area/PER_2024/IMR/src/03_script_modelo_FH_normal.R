#################################################
#             Proyecto : SAEfertility           #
#             Modelo fay harriot normal         #
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

est_dir <- readRDS(
  "Modelo_area/PER_2024/IMR/output/estimación_directa_provi_IMR.rds"
)

est_dir <- est_dir %>%
  mutate(provi = as.character(provincia),
         provi = if_else(nchar(provi) == 3, str_c("0", provi), provi))

info_covariables <- readRDS("Modelo_area/PER_2024/IMR/input/statelevel_predictors_df_update_provi_muj.rds") %>% mutate(dam = str_sub(provi, 1, 2))

info_satelitales <- readRDS("Modelo_area/PER_2024/IMR/input/satelitales_provincia.rds") %>%
  rename(
    provi = provi,
    luces_nocturnas = F182013_stable_lights ,
    indice_mod_humano = X2016_gHM,
    cubrimiento_urbano = X2016_urban.coverfraction,
    cubrimiento_rural = X2016_crops.coverfraction
  ) %>% mutate(dam = str_sub(provi, 1, 2)) %>% 
  group_by(dam) %>%
  mutate(
    luces_nocturnas = ifelse(
      is.na(luces_nocturnas),
      mean(luces_nocturnas, na.rm = TRUE),
      luces_nocturnas
    ),
    indice_mod_humano = ifelse(
      is.na(indice_mod_humano),
      mean(indice_mod_humano, na.rm = TRUE),
      indice_mod_humano
    ),
    cubrimiento_urbano = ifelse(
      is.na(cubrimiento_urbano),
      mean(cubrimiento_urbano, na.rm = TRUE),
      cubrimiento_urbano
    ),
    cubrimiento_rural = ifelse(
      is.na(cubrimiento_rural),
      mean(cubrimiento_rural, na.rm = TRUE),
      cubrimiento_rural
    )
  ) %>%
  ungroup()

info_covariables <- info_covariables %>%
  left_join(info_satelitales %>% dplyr::select(-c("dam")),   by = "provi") 

# Normalización global (min y max) variable por variable
vars <- c("luces_nocturnas","indice_mod_humano",
          "cubrimiento_urbano","cubrimiento_rural", "mean_hijos")


info_covariables <- info_covariables %>%
  ungroup() %>%
  mutate(across(all_of(vars), ~ {
    mn <- min(., na.rm = TRUE)
    mx <- max(., na.rm = TRUE)
    if (is.finite(mn) & is.finite(mx) & mx > mn) {
      (. - mn) / (mx - mn)
    } else {
      NA_real_
    }
  }))

#Crear dummies por dpto

info_covariables <- dummy_cols(info_covariables, select_columns = "dam",remove_selected_columns = TRUE)

################################################################################
###------------------------Modelo FAY HARRIOT NORMAL-------------------------###
################################################################################

#Union full entre encuesta y covariables administrativas
base_FH <- full_join(est_dir, info_covariables, by = "provi" )
summary(base_FH)
saveRDS(base_FH, "Modelo_area/PER_2024/IMR/output/Base_FH.rds")

##Preparando los insumos para STAN-----------------------------------------------

#Dominios observados
data_dir <- base_FH %>% filter(!is.na(IMR))
#Dominios no observados
data_syn <-
  base_FH %>% anti_join(data_dir %>% dplyr:: select(provi))

#modelo1, full - definiendo matriz de efectos fijos

nombres <- names(info_covariables)
nombres <- nombres[!(nombres %in% c("provi", "depto"))]


my_formula1_full <- as.formula(                      
  paste(" ~ ", 
        paste(nombres, 
              collapse = " + ")))

#dominios  observados
Xdat <- model.matrix(my_formula1_full, data = data_dir)
#dominios no observados
Xs <- model.matrix(my_formula1_full, data = data_syn)

#Identificar columnas de Xdat que no estan presentes en Xs
temp <- setdiff(colnames(Xdat),colnames(Xs))

#Creando lista de parametros para STAN
sample_data <- list(
  N1 = nrow(Xdat),   # Observados.
  N2 = nrow(Xs),   # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  y  = as.numeric(data_dir$IMR), # Estimación directa
  sigma_e = sqrt(data_dir$vardir)  # Error de estimación
)

fit_FH_normal <- "Modelo_area/PER_2024/0funciones/17FH_normal.stan"
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # speed up running time 
model_FH_normal <- stan(
  file = fit_FH_normal,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 9000,   #iter - 1000      
  iter = 10000,            
  cores = 4,
  chains = 2,
  seed = 11082025,
  control = list(
    adapt_delta   = 0.99,
    max_treedepth = 10
  )
  
  )

saveRDS(object = model_FH_normal,
        file = "Modelo_area/PER_2024/IMR/output/model_FH_normal.rds")

y_pred_B <- as.array(model_FH_normal, pars = "theta") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 500)
y_pred2 <- y_pred_B[rowsrandom, ]#de las 4k toma 500 lineas aleatorias
a <- ppc_dens_overlay(y = as.numeric(data_dir$hijos_nacidos), y_pred2)

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/IMR/output/modelo_FH_normal.jpeg", 
       scale = 3)

#Analisis del grafico de la convergencia de las cadenas de sigma cuadrado_u
posterior_sigma2_u <- as.array(model_FH_normal, pars = "sigma2_u")
(mcmc_dens_chains(posterior_sigma2_u) +
    mcmc_areas(posterior_sigma2_u) ) / 
  mcmc_trace(posterior_sigma2_u)
validacionRhat<-summary(model_FH_normal)$summary%>%as.data.frame()
mcmc_rhat(validacionRhat$Rhat)
validacionRhat%>%filter(Rhat>1.1) #aumentar iteraciones, ideal Rhat<1.1

#metodo de validacion del modelo FH obtenido en STAN
theta <-   summary(model_FH_normal, pars =  "theta")$summary %>%
  data.frame()
thetaSyn <-   summary(model_FH_normal, pars =  "thetaSyn")$summary %>%
  data.frame()
theta_FH <-   summary(model_FH_normal, pars =  "thetaFH")$summary %>%
  data.frame()

data_dir %<>% mutate(
  thetadir = hijos_nacidos,
  theta_pred = theta$mean,
  thetaSyn = thetaSyn$mean,
  thetaFH = theta_FH$mean,
  theta_pred_EE = theta$sd,
  Cv_theta_pred = theta_pred_EE/theta_pred,
  doble_theta_pred_EE = theta_pred_EE*2
) 

# Estimación predicción del modelo vs ecuación ponderada de FH
p11 <- ggplot(data_dir, aes(x = theta_pred, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación sintética
p12 <- ggplot(data_dir, aes(x = thetaSyn, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación directa
p21 <- ggplot(data_dir, aes(x = thetadir, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación directa Vs estimación sintética
p22 <- ggplot(data_dir, aes(x = thetadir, y = thetaSyn)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10)

a <- (p11+p12)/(p21+p22)

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/IMR/output/modelo_FH_normal_comparaciones.jpeg", 
       scale = 3)

#Estimacion del FH en los dominios NO observados
theta_syn_pred <- summary(model_FH_normal, pars =  "y_pred")$summary %>%
  data.frame()

data_syn <- data_syn %>% 
  mutate(
    theta_pred = theta_syn_pred$mean,
    thetaSyn = theta_pred,
    thetaFH = theta_pred,
    theta_pred_EE = theta_syn_pred$sd,
    Cv_theta_pred = theta_pred_EE/theta_pred,
    doble_theta_pred_EE = theta_pred_EE*2)
dim(data_syn)
dim(theta_syn_pred)

data_dir2<-data_dir %>% dplyr::select(dame,
                                      hijos_nacidos,thetaFH,
                                      theta_pred,theta_pred_EE,
                                      Cv_theta_pred,
                                      doble_theta_pred_EE)
data_syn2<-data_syn %>% dplyr::select(dame,
                                      hijos_nacidos,thetaFH,
                                      theta_pred,theta_pred_EE,
                                      Cv_theta_pred,
                                      doble_theta_pred_EE)

data_estimacion<-rbind(data_dir2,data_syn2) 
names(data_estimacion)
dim(data_estimacion)


#Guardar la base de las estimaciones
saveRDS(data_estimacion, "Modelo_area/PER_2024/IMR/output/estimacion_promedio_hij_vivos_FH.rds")

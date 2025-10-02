#################################################
#             Proyecto : SAEfertility           #
#             Modelo beta inflado               #
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
  "Modelo_area/PER_2024/U5MR/output/estimación_directa_phv.rds"
)
est_dir <- est_dir %>%
  mutate(dame = as.character(dame),
         dame = if_else(nchar(dame) == 5, str_c("0", dame), dame))

# info_covariables <- readRDS(
#   "Modelo_area/PER_2024/U5MR/input/statelevel_predictors_df_update_dame.rds"
# )  %>% mutate(dam = str_sub(dame, 1, 2))

info_covariables <- readRDS("Modelo_area/PER_2024/U5MR/input/statelevel_predictors_df_update_dame_muj.rds") %>% mutate(dam = str_sub(dame, 1, 2))

info_satelitales <- readRDS("Modelo_area/PER_2024/U5MR/input/satelitales_dame.rds") %>%
  dplyr :: select(-c("tasa_desocupacion")) %>%
  rename(
    dame = mpio,
    luces_nocturnas = F182013_stable_lights ,
    indice_mod_humano = X2016_gHM,
    accesibilidad_hospitales = accessibility,
    accesibilidad_hosp_caminando = accessibility_walking_only,
    cubrimiento_urbano = X2016_urban.coverfraction,
    cubrimiento_rural = X2016_crops.coverfraction
  ) %>%
  group_by(depto) %>%
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
    accesibilidad_hospitales = ifelse(
      is.na(accesibilidad_hospitales),
      mean(accesibilidad_hospitales, na.rm = TRUE),
      accesibilidad_hospitales
    ),
    accesibilidad_hosp_caminando = ifelse(
      is.na(accesibilidad_hosp_caminando),
      mean(accesibilidad_hosp_caminando, na.rm = TRUE),
      accesibilidad_hosp_caminando
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
  left_join(info_satelitales %>% dplyr::select(-c("depto")),   by = "dame") 

# Normalización global (min y max) variable por variable
vars <- c("luces_nocturnas","indice_mod_humano",
          "accesibilidad_hospitales","accesibilidad_hosp_caminando",
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
###------------------------    Modelo BETA INFLADO  -------------------------###
################################################################################

#Union full entre encuesta y covariables administrativas
base_FH <- full_join(est_dir, info_covariables, by = "dame" )

saveRDS(base_FH, "Modelo_area/PER_2024/U5MR/output/Base_FH.rds")

##Preparando los insumos para STAN-----------------------------------------------

#Dominios observados
data_dir <- base_FH %>% filter(!is.na(U5MR))
#Dominios no observados
data_syn <-
  base_FH %>% anti_join(data_dir %>% dplyr:: select(dame))

#modelo1, full - definiendo matriz de efectos fijos

nombres <- names(info_covariables)
nombres <- nombres[!(nombres %in% c("dame", "depto"))]


my_formula1_full <- as.formula(                      
  paste(" ~ ", 
        paste(nombres, 
              collapse = " + ")))

#dominios  observados
Xdat <- model.matrix(my_formula1_full, data = data_dir)
#dominios no observados
Xs <- model.matrix(my_formula1_full, data = data_syn)

#Identificar columnas de Xdat que no estan presentes en Xshttp://127.0.0.1:28145/graphics/plot_zoom_png?width=1076&height=765
temp <- setdiff(colnames(Xdat),colnames(Xs))


fit_beta <- "Modelo_area/PER_2024/0funciones/20_beta_inflado_v2.stan"

options(mc.cores = parallel::detectCores())

rstan::rstan_options(auto_write = TRUE) # speed up running time 


sample_data <- list(
  N1 = nrow(Xdat),   # Observados.
  N2 = nrow(Xs),   # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  y  = as.numeric(data_dir$U5MR), # Estimación directa
  V = data_dir$vardir  
)

model_betaa <- stan(
  file = fit_beta,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 9000,   #iter - 1000      
  iter = 10000,            
  cores = 4,
  chains = 4,
  seed = 11082025,
  control = list(
    adapt_delta   = 0.99,
    max_treedepth = 12
  )
  
)

saveRDS(object = model_betaa,
        file = "Modelo_area/PER_2024/U5MR/output/model_beta_inflado_3.rds")

model_betaa <- readRDS(file = "Modelo_area/PER_2024/U5MR/output/model_beta_inflado_3.rds")



y_pred_B <- as.array(model_betaa, pars = "z_obs") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 200)
y_pred2 <- y_pred_B[rowsrandom, ]#de las 4k toma 500 lineas aleatorias
a <- ppc_dens_overlay(y = as.numeric(data_dir$U5MR), y_pred2)


#Analisis del grafico de la convergencia de las cadenas de sigma cuadrado_u
posterior_sigma2_u <- as.array(model_betaa, pars = "sigma_u")
(mcmc_dens_chains(posterior_sigma2_u) +
    mcmc_areas(posterior_sigma2_u) ) / 
  mcmc_trace(posterior_sigma2_u)
validacionRhat<-summary(model_betaa)$summary%>%as.data.frame()
mcmc_rhat(validacionRhat$Rhat)
validacionRhat%>%filter(Rhat>1.1) #aumentar iteraciones, ideal Rhat<1.1


#Extraccion de las estimaciones dominios observados
theta_obs <-   summary(model_betaa, pars =  "z_obs")$summary %>%
  data.frame()
data_dir %<>% mutate(
  thetadir = U5MR,
  theta_pred = theta_obs$mean * 1000,
  theta_pred_EE = theta_obs$sd * 1000,
  Cv_theta_pred = theta_pred_EE/theta_pred,
  doble_theta_pred_EE = theta_pred_EE*2
) 


#Estimacion del FH en los dominios NO observados
theta_nobs <-   summary(model_betaa, pars =  "y_pred")$summary %>%
  data.frame()

data_syn <- data_syn %>% 
  mutate(
    theta_pred = theta_nobs$mean * 1000,
    theta_pred_EE = theta_nobs$sd*1000,
    Cv_theta_pred = theta_pred_EE/theta_pred,
    doble_theta_pred_EE = theta_pred_EE*2)

dim(data_syn)

data_dir2<-data_dir %>% dplyr::select(dame,
                                      U5MR,
                                      theta_pred,theta_pred_EE,
                                      Cv_theta_pred,
                                      doble_theta_pred_EE)
data_syn2<-data_syn %>% dplyr::select(dame,
                                      U5MR,
                                      theta_pred,theta_pred_EE,
                                      Cv_theta_pred,
                                      doble_theta_pred_EE)

data_estimacion<-rbind(data_dir2,data_syn2) 

names(data_estimacion)
dim(data_estimacion)

## Qqplot de los efectos aleatorios

efectos_aleatorios <-   summary(model_betaa, pars =  "u")$summary %>%
  data.frame()

p1 <- ggplot(data = data.frame(y = efectos_aleatorios$mean), aes(x = y)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 15,
    fill = "skyblue",
    color = "white",
    alpha = 0.7
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(efectos_aleatorios$mean), sd = sd(efectos_aleatorios$mean)),
    color = "red",
    size = 1
  ) +
  labs(title = "Distribución de los efectos aleatorios", x = "Efectos aleatorios", y = "Densidad") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank())


p2 <- ggplot(data = data.frame(y = efectos_aleatorios$mean), aes(sample = y)) +
  stat_qq(color = "steelblue", size = 1) +
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Q-Q Plot efectos aleatorios",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )


p_comb <- p1 | p2 ; p_comb

ggsave(plot = p_comb,
       filename =  "Modelo_area/PER_2024/U5MR/output/modelo_beta_efectos_aleatorios.jpeg",
       scale = 5)

########################### Supuestos residuales   #############################

sigma_u <-   summary(model_betaa, pars =  "sigma_u")$summary %>%
  data.frame()

residual_stan <-  (theta_obs$mean - theta_nobs$mean)/sqrt(sigma_u$mean + data_dir$vardir)
predi_residuales <- theta_obs$mean/sqrt(sigma_u$mean + data_dir$vardir)

p1 <- ggplot(data = data.frame(y = residual_stan), aes(x = y)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 15,
    fill = "skyblue",
    color = "white",
    alpha = 0.7
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(residual_stan), sd = sd(residual_stan)),
    color = "red",
    size = 1
  ) +
  labs(title = "Distribución de los residuales", x = "Residual", y = "Densidad") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank())


p2 <- ggplot(data = data.frame(y = residual_stan), aes(sample = y)) +
  stat_qq(color = "steelblue", size = 1) +
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Q-Q Plot de los residuales",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

p3 <- ggplot(data = data.frame(
  id = 1:length(residual_stan), 
  residuales = residual_stan),
  aes(x = id, y = residuales)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    x = "Distritos",
    y = "Residuales estandarizados") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_comb <- (p1|p2)/p3 ; p_comb

ggsave(plot = p_comb,
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_residuales.jpeg",
       scale = 4)


#Guardar la base de las estimaciones
saveRDS(data_estimacion, "Modelo_area/PER_2024/U5MR/output/estimacion_U5MR_FH_3.rds")

#################################################
#             Proyecto : SAEfertility           #
#             Modelo binomial                   #
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
###----------------------------- Loading datasets --------------------------###
################################################################################

est_dir <- readRDS(
  "Modelo_area/PER_2024/ASFR/45-49/output/estimación_directa_phv.rds"
)

est_dir <- est_dir %>%
  mutate(provi = as.character(provi),
         provi = if_else(nchar(provi) == 3, str_c("0", provi), provi),
         ASFR_2 = ASFR/1000,
         deff_aj = if_else(deff <1 | is.na(deff) , 1, deff) )

info_covariables <- readRDS("Modelo_area/PER_2024/ASFR/45-49/input/statelevel_predictors_df_update_provi_muj.rds") %>% mutate(dam = str_sub(provi, 1, 2))

info_satelitales <- readRDS("Modelo_area/PER_2024/ASFR/45-49/input/satelitales_provincia.rds") %>%
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
###------------------------    Modelo Binomial      -------------------------###
################################################################################

#Union full entre encuesta y covariables administrativas
base_FH <- full_join(est_dir, info_covariables, by = "provi" )


##Preparando los insumos para STAN-----------------------------------------------

#Dominios observados
data_dir <- base_FH %>% filter(!is.na(ASFR))
#Dominios no observados
data_syn <-
  base_FH %>% anti_join(data_dir %>% dplyr:: select(provi))

#modelo1, full - definiendo matriz de efectos fijos

nombres <- names(info_covariables)
nombres <- nombres[!(nombres %in% c("provi", "depto"))]
nombres <- nombres[!(nombres %in% c("provi", "dam","depto", "area1", "tasa_empleador_m","dam_25","anoest2",
                                    "anoest4", "rezago_escolar_m","analfabeta_m","carece_computadora_m"))]

formula_mod <- as.formula(                      
  paste(" ~ ", 
        paste(nombres, 
              collapse = " + ")))


## Dominios observados
Xdat <- model.matrix(formula_mod, data = data_dir)

## Dominios no observados
Xs <- model.matrix(formula_mod, data = data_syn)

n_effec = ceiling(data_dir$nd/data_dir$deff_aj)
#n_effec = ceiling(data_dir$nd)
y_effect  = ceiling((data_dir$ASFR_2)*n_effec)

sample_data <- list(
  N1 = nrow(Xdat),   # Observados.
  N2 = nrow(Xs),   # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  n_effec = n_effec,
  y_effect  = y_effect          # Estimación directa. 
)

fit_FH_binomial    <- "Modelo_area/PER_2024/0funciones/14FH_binomial.stan"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) 

model_FH_Binomial  <- stan(
  file = fit_FH_binomial  ,  
  data = sample_data,   
  verbose = TRUE,
  warmup = 19000,         
  iter = 20000,            
  cores = 4,
  chains = 4,
  seed = 01102025,
  open_progress = TRUE,
  control = list(
    adapt_delta   = 0.99,
    max_treedepth = 10
  )
  
)

saveRDS(object = model_FH_Binomial,
        file = "Modelo_area/PER_2024/ASFR/45-49/output/model_binomial_02102025.rds")

model_FH_Binomial <- readRDS(file = "Modelo_area/PER_2024/ASFR/45-49/output/model_binomial_01102025.rds")

rango <- 11001:15000

y_pred_B <- as.array(model_FH_Binomial, pars = "theta") %>%
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 200)
y_pred2 <- y_pred_B[rowsrandom, ]#de las 4k toma 500 lineas aleatorias
a <- ppc_dens_overlay(y = as.numeric(data_dir$ASFR_2), y_pred2)
a
ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/ASFR/45-49/output/modelo_binomial_density.jpeg",
       scale = 5)


#Analisis del grafico de la convergencia de las cadenas de sigma cuadrado_u
posterior_sigma_u <- as.array(model_FH_Binomial, pars = "sigma_u")
b <- (mcmc_dens_chains(posterior_sigma_u) +
        mcmc_areas(posterior_sigma_u) ) /
  mcmc_trace(posterior_sigma_u)
b
ggsave(plot = b,
       filename =  "Modelo_area/PER_2024/ASFR/45-49/output/modelo_binomial_density_sigma.jpeg",
       scale = 5)

validacionRhat<-summary(model_FH_Binomial)$summary%>%as.data.frame()
mcmc_rhat(validacionRhat$Rhat)
validacionRhat%>%filter(Rhat>1.1) #aumentar iteraciones, ideal Rhat<1.1

#Analisis de la convergencia de las cadenas de theta
posterior_theta <- as.array(model_FH_Binomial, pars = "thetaLP") %>% as_draws_matrix()
posterior_theta <- posterior_theta[rango, ]
saveRDS(object = posterior_theta,
        file = "Modelo_area/PER_2024/ASFR/45-49/Cadenas_thetapred_01102025.rds")


theta_FH_pred <- summary(posterior_theta) %>%
  data.frame()

options(scipen = 999)
data_estimation <- bind_rows(data_dir, data_syn) %>%
  mutate(theta_pred = theta_FH_pred$mean,
         theta_pred_EE = theta_FH_pred$sd,
         Cv_pred = theta_pred_EE/theta_pred) %>% dplyr::select(provi,
                                                               ASFR_2,
                                                               theta_pred,theta_pred_EE,
                                                               Cv_pred)


## Qqplot de los efectos aleatorios
efectos_aleatorios <- as.array(model_FH_Binomial, pars = "u") %>% as_draws_matrix()
efectos_aleatorios <-   summary(efectos_aleatorios) %>%
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
       filename =  "Modelo_area/PER_2024/ASFR/45-49/output/modelo_beta_efectos_aleatorios.jpeg",
       scale = 5)


#Guardar la base de las estimaciones
saveRDS(data_estimation, "Modelo_area/PER_2024/ASFR/45-49/output/estimacion_ASFR_binomial_01102025.rds")



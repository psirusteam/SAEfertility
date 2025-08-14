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

est_dir <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_phv_FGV.rds"
)
est_dir <- est_dir %>%
  mutate(dame = as.character(dame),
         dame = if_else(nchar(dame) == 5, str_c("0", dame), dame))

info_covariables <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/input/statelevel_predictors_df_dame.rds"
) %>% rename(dame = dam2)

censo_hijos_nc <- readRDS(
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/censo_hijos_nacidos.rds"
)

info_satelitales <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/input/satelitales_dame.rds") %>%
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

# Normalización global (media 0, sd 1) variable por variable
vars <- c("luces_nocturnas","indice_mod_humano",
          "accesibilidad_hospitales","accesibilidad_hosp_caminando",
          "cubrimiento_urbano","cubrimiento_rural")

info_satelitales <- info_satelitales %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(vars), ~{
      m <- mean(., na.rm=TRUE); s <- sd(., na.rm=TRUE)
      if (is.na(s) || s == 0) 0 else (.-m)/s
    })
  )

info_covariables <- left_join(info_covariables, info_satelitales, by = "dame") %>% 
  left_join(censo_hijos_nc %>% select(-c("dam")) , by = "dame")

#Crear dummies por dpto
info_covariables <- fastDummies::dummy_cols(info_covariables,
                                            select_columns = "depto",
                                            remove_selected_columns = TRUE)

################################################################################
###------------------------Modelo FAY HARRIOT NORMAL-------------------------###
################################################################################

#Union full entre encuesta y covariables administrativas
base_FH <- full_join(est_dir, info_covariables, by = "dame" )

saveRDS(base_FH, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/Base_FH.rds")

##Preparando los insumos para STAN-----------------------------------------------

#Dominios observados
data_dir <- base_FH %>% filter(!is.na(hijos_nacidos))
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

#Identificar columnas de Xdat que no estan presentes en Xs
temp <- setdiff(colnames(Xdat),colnames(Xs))

#Creando lista de parametros para STAN
sample_data <- list(
  N1 = nrow(Xdat),   # Observados.
  N2 = nrow(Xs),   # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  y  = as.numeric(data_dir$hijos_nacidos), # Estimación directa
  sigma_e = sqrt(data_dir$hat_var)  # Error de estimación
)

fit_FH_normal <- "Modelo_area/PER_2024/0funciones/17FH_normal.stan"
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # speed up running time 
model_FH_normal <- stan(
  file = fit_FH_normal,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 11000,   #iter - 1000      
  iter = 12000,            
  cores = 4,
  seed = 11082025,
  control = list(
    adapt_delta   = 0.99,
    max_treedepth = 12
  ))

saveRDS(object = model_FH_normal,
        file = "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/model_FH_normal_2.rds")

model_FH_normal <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/model_FH_normal_2.rds")


# Grafico de las predicciones
y_pred_B <- as.array(model_FH_normal, pars = "theta") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 500)
y_pred2 <- y_pred_B[rowsrandom, ]#de las 4k toma 500 lineas aleatorias
a <- ppc_dens_overlay(y = as.numeric(data_dir$hijos_nacidos), y_pred2)

ggsave(plot = a,
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_2.jpeg", 
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

sigma2_u <-   summary(model_FH_normal, pars =  "sigma2_u")$summary %>%
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

## Qqplot de los efectos aleatorios

efectos_aleatorios <-   summary(model_FH_normal, pars =  "u")$summary %>%
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
  labs(title = "Distribución de los efectos aleatorios", x = "Residual", y = "Densidad") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank())


p2 <- ggplot(data = data.frame(y = efectos_aleatorios$mean), aes(sample = y)) +
  stat_qq(color = "steelblue", size = 1) +
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Q-Q Plot efectos aleatorios vs. Normal estándar",
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
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_efectos_aleatorios_2.jpeg",
       scale = 5)


########################### Supuestos residuales   #############################
residual_stan <-  (theta_FH$mean - thetaSyn$mean)/sqrt(sigma2_u$mean + data_dir$hat_var)
predi_residuales <- thetaSyn$mean/sqrt(sigma2_u$mean + data_dir$hat_var)

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
    title = "Q-Q Plot vs. Normal estándar",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

p3 <- ggplot(data = data.frame(y = residual_stan,
                       
                         id = 1:length(predi_residuales))) +
  geom_point(aes(y = y , x = id)) + 
  geom_hline(yintercept  = 0) + theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

(p1|p2)/p3
p_comb <- (p1|p2)/p3 ; p_comb

ggsave(plot = p_comb,
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_residuales_2.jpeg",
       scale = 5)



################################################################################

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
       filename =  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/modelo_FH_normal_comparaciones_2.jpeg", 
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
saveRDS(data_estimacion, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimacion_promedio_hij_vivos_FH_2.rds")

ggplot(data_estimacion, aes(x = dame, y = thetaFH)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "",
       x = "dame",
       y = "thetaFH") +
  theme_minimal()

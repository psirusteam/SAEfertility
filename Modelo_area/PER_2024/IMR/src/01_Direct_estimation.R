#################################################
#             Proyecto : SAEfertility           #
#       Direct estimation        - PERU         #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(tidyr)
library(writexl)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_nacimientos <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/base_nacimientos.rds")


################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################

# --------------------------------------#
#        Infant  mortality rate         #
# --------------------------------------#

# Inicializar lista para resultados dame
resultados_por_dame <- list()

base_imr <- base_nacimientos %>%
  filter(!is.na(date_nac_hij)) %>%
  mutate(
    rweight = fep_m / 1e6,             #  Ponderador 
    tu = date_entrevista,              # Fecha entrevista 
    tl = date_entrevista - 60          # 5 años antes (periodo de referencia)
  )

segmentos <- list("1" = c(0, 1),"2" = c(1, 3),"3" = c(3, 6),"4" = c(6, 12),"5" = c(12, 24),"6" = c(24, 36),"7" = c(36, 48),"8" = c(48, 60))
# Inicializamos vector de resultados

q_est <- numeric()
se_est <- numeric()


# Iterar por cada grupo dame
for (grupo in unique(base_imr$dame)) {
  
  base_dame <- base_imr %>% filter(dame == grupo)
  
  q_est <- numeric()
  se_est <- numeric()
  
  for (i in names(segmentos)) {
    a1 <- segmentos[[i]][1]
    a2 <- segmentos[[i]][2]
    
    seg <- base_dame %>%
      filter(is.na(edad_muerte_imp) | edad_muerte_imp >= a1) %>%
      mutate(
        exposure = case_when(
          date_nac_hij >= (tl - a2) & date_nac_hij < (tl - a1) ~ 0.5,
          date_nac_hij >= (tl - a1) & date_nac_hij < (tu - a2) ~ 1,
          date_nac_hij >= (tu - a2) & date_nac_hij < (tu - a1) ~ 0.5,
          TRUE ~ 0
        ),
        death = case_when(
          date_nac_hij >= (tl - a2) & date_nac_hij < (tl - a1) & edad_muerte_imp >= a1 & edad_muerte_imp < a2 ~ 0.5,
          date_nac_hij >= (tl - a1) & date_nac_hij < (tu - a2) & edad_muerte_imp >= a1 & edad_muerte_imp < a2 ~ 1,
          date_nac_hij >= (tu - a2) & date_nac_hij < (tu - a1) & edad_muerte_imp >= a1 & edad_muerte_imp < a2 ~ ifelse(tu, 1, 0.5),
          is.na(edad_muerte_imp) ~ 0,
          TRUE ~ 0
        )
      ) %>%
      filter(exposure > 0)
    
    disenio <- svydesign(
      id = ~upm,
      strata = ~strata,
      weights = ~rweight,
      data = seg,
      nest = TRUE
    )
    
    ratio <- svyratio(~death, ~exposure, disenio)
    q_est[i] <- coef(ratio)
    se_est[i] <- SE(ratio)
  }
  
  # Calcular IMR, CMR, U5MR
  calc_mort <- function(indices) {
    est <- (1 - prod(1 - q_est[indices])) * 1000
    partials <- sapply(indices, function(j) prod(1 - q_est[setdiff(indices, j)]))
    se <- 1000 * sqrt(sum((partials^2) * se_est[indices]^2))
    cv <- (se / est) * 100
    return(c(est = est, se = se, cv = cv))
  }
  
  imr <- calc_mort(1:4)
  cmr <- calc_mort(5:8)
  u5mr <- calc_mort(1:8)
  
  resultados_por_dame[[as.character(grupo)]] <- data.frame(
    dame = grupo,
    IMR = imr["est"], se_IMR = imr["se"], cv_IMR = imr["cv"],
    CMR = cmr["est"], se_CMR = cmr["se"], cv_CMR = cmr["cv"],
    U5MR = u5mr["est"], se_U5MR = u5mr["se"], cv_U5MR = u5mr["cv"]
  )
}

#Step 1. Agrupar la base de nacimiento por mujeres

base_nacxmuj <- base_nacimientos %>% filter(edad_actual >= 15 & edad_actual <=49) %>% 
  group_by(id_individual) %>%
  summarise(hijos_nacidos = n())

#Step 2. Unir a la base de mujeres

base_MEF <- base_MEF %>%
  left_join(base_nacxmuj, by = "id_individual")  %>%
  mutate(hijos_nacidos = ifelse(is.na(hijos_nacidos), 0, hijos_nacidos))

base_MEF$fep_m <- base_MEF$fep_m/1000000

#Step 3. Definir el objeto svydesign
options(survey.lonely.psu = "adjust")

diseno <- svydesign(
  id = ~upm,
  strata = ~strata,
  weights = ~fep_m,
  data = base_MEF,
  nest = TRUE
)

svymean(~hijos_nacidos, diseno, vartype = c("se", "cv"), na.rm = TRUE)
estimacion <- svyby(~hijos_nacidos,by = ~dam + dame,design = diseno,FUN = svymean,deff = TRUE,na.rm = TRUE)

nupm <- base_MEF %>% distinct(dam,dame,upm) %>%
  group_by(dam,dame) %>% 
  tally()

nd <- base_MEF %>%
  group_by(dam, dame) %>%
  summarise(nd = n(), .groups = "drop")

estimacion <- estimacion %>% left_join(
  nupm,  by = c("dam", "dame")) %>% left_join(
  nd, by = c("dam", "dame")) %>% rename(p_deff = DEff.hijos_nacidos)



#Estimacion directa al nivel de representatividad de la encuesta para benchmaring
estimacion_dam <- svyby(
  ~ hijos_nacidos,
  by = ~ dam,
  design = diseno,
  FUN = svymean,
  deff = TRUE,
  na.rm = TRUE
)

saveRDS(
  estimacion_dam,
  "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_dam.rds"
)

#CRITERIOS DE CALIDAD------------------------------------
##Excluyendo registros por falta de calidad
#base datos excluyendo los registros que cumplen criterios de calidad para 
#modelo sae

base_sae <- estimacion %>% data.frame()%>%
  filter(nd > 40, p_deff > 1, n >= 2) %>%
  transmute(
    dam = dam,              # Id para los departamento
    dame = dame,              #Id para los distritos
    nd = nd,                # Número de observaciones por dominios
    hijos_nacidos = hijos_nacidos,      # Estimación de la variable
    vardir = se ^ 2,      # Estimación de la varianza directa 
    cv = se/hijos_nacidos,                       
    deff_muni = p_deff        # Deff por dominio municipal
  )

saveRDS(base_sae, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_phv.rds")

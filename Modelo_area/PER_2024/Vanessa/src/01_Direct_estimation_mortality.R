#################################################
#             Proyecto : SAEfertility           #
# Direct estimation Mortality  bases - PERU     #
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
library(lubridate)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")


base_nacimientos <- readRDS(file.path(output, "base_nacimientos.rds"))

################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################

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

# Iterar por segmentos
for (i in names(segmentos)) {
  a1 <- segmentos[[i]][1]
  a2 <- segmentos[[i]][2]
  
  # Construimos base con lógica DHS
  seg <- base_imr %>%
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
  
  # Diseño muestral
  disenio <- svydesign(
    id = ~upm,
    strata = ~strata,
    weights = ~rweight,
    data = seg,
    nest = TRUE
  )
  
  # Ratio con svyratio()
  ratio <- svyratio(~death, ~exposure, disenio)
  
  q_est[i] <- coef(ratio)
  se_est[i] <- SE(ratio)
}



# --------------------------------------#
#        Infant  mortality rate         #
# --------------------------------------#

# IMR
IMR <- (1 - prod(1 - q_est[1:4])) * 1000

# Error estándar
partials <- sapply(1:4, function(j) prod(1 - q_est[-j]))
se_imr <- 1000 * sqrt(sum((partials^2) * se_est[1:4]^2))

# Coeficiente de variación
cv_imr <- (se_imr / IMR) * 100

# --------------------------------------#
#      Under-five mortality rate        #
# --------------------------------------#

U5MR <- (1 - prod(1 - q_est[1:8])) * 1000

# se
partials <- sapply(1:4, function(j) prod(1 - q_est[-j]))
se_u5mr <- 1000 * sqrt(sum((partials^2) * se_est[1:8]^2))

# cv
cv_u5mr <- (se_u5mr / U5MR) * 100

# --------------------------------------#
#         Child Mortaliry Rate          #
# --------------------------------------#

CMR <- (1 - prod(1 - q_est[5:8])) * 1000

# se
partials <- sapply(1:4, function(j) prod(1 - q_est[-j]))
se_cmr <- 1000 * sqrt(sum((partials^2) * se_est[5:8]^2))

# cv
cv_cmr <- (se_cmr / IMR) * 100

# --------------------------------------#
#           Total Results               #
# --------------------------------------#

df_mortalidad <- data.frame(
  Indicador = c("IMR", "U5MR", "CMR"),
  Estimacion = c(IMR, U5MR, CMR),
  SE = c(se_imr, se_u5mr, se_cmr),
  CV = c(cv_imr, cv_u5mr, cv_cmr)
)

write_xlsx(df_mortalidad, path = file.path(output, "Mortality/Mortality_indicators_total.xlsx"))


######################################## dam


# Inicializar lista para resultados dam
resultados_por_dam <- list()

# Iterar por cada grupo dam
for (grupo in unique(base_imr$dam)) {
  
  base_dam <- base_imr %>% filter(dam == grupo)
  
  q_est <- numeric()
  se_est <- numeric()
  
  for (i in names(segmentos)) {
    a1 <- segmentos[[i]][1]
    a2 <- segmentos[[i]][2]
    
    seg <- base_dam %>%
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
  
  resultados_por_dam[[as.character(grupo)]] <- data.frame(
    dam = grupo,
    IMR = imr["est"], se_IMR = imr["se"], cv_IMR = imr["cv"],
    CMR = cmr["est"], se_CMR = cmr["se"], cv_CMR = cmr["cv"],
    U5MR = u5mr["est"], se_U5MR = u5mr["se"], cv_U5MR = u5mr["cv"]
  )
}

# Consolidar en un solo data.frame
df_mortalidad_dam <- bind_rows(resultados_por_dam)


write_xlsx(df_mortalidad_dam, path = file.path(output, "Mortality/Mortality_indicators_dam.xlsx"))

######################################## dam - sex

resultados_dam_sex <- list()
subgrupos <- unique(base_imr[c("dam", "sex_hij")])


# Iterar por cada grupo dam
for (k in 1:nrow(subgrupos)) {
  dam_actual <- subgrupos$dam[k]
  sexo_actual <- subgrupos$sex_hij[k]
  
  base_sub <- base_imr %>% filter(dam == dam_actual, sex_hij == sexo_actual)
  
  q_est <- numeric()
  se_est <- numeric()
  
  for (i in names(segmentos)) {
    a1 <- segmentos[[i]][1]
    a2 <- segmentos[[i]][2]
    
    seg <- base_sub %>%
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
  
  resultados_dam_sex[[paste0(dam_actual, "_", sexo_actual)]] <- data.frame(
    dam = dam_actual,
    sexo_hij = sexo_actual,
    IMR = imr["est"], se_IMR = imr["se"], cv_IMR = imr["cv"],
    CMR = cmr["est"], se_CMR = cmr["se"], cv_CMR = cmr["cv"],
    U5MR = u5mr["est"], se_U5MR = u5mr["se"], cv_U5MR = u5mr["cv"]
  )
}

df_mortalidad_dam_sex <- bind_rows(resultados_dam_sex)


write_xlsx(df_mortalidad_dam_sex, path = file.path(output, "Mortality/Mortality_indicators_dam_sex.xlsx"))


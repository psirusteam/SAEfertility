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
library(DHS.rates)
library(purrr)
library(tibble)
library(stringr)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_MEF <- read.csv("Modelo_area/PER_2024/ASFR/15-19/input/REC0111_2024.csv") # Modulo mujeres en edad fertil 12 - 49 años
base_MEF <- base_MEF %>% filter(!is.na(V101)) %>% mutate(
  UBIGEO = str_pad(
    string = UBIGEO, 
    width = 6,           
    side = "left",      
    pad = "0"           
  ),
  provincia = str_sub(UBIGEO, start = 1, end = 4))
base_emb <- read.csv("Modelo_area/PER_2024/ASFR/15-19/input/REC21_2024.csv") #Historial de embarazos


################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################

base_emb_anch <- base_emb %>%
  group_by(CASEID) %>%
  arrange(CASEID, B3) %>%
  mutate(b3_n = paste0("b3_", sprintf("%02d", row_number()))) %>%
  ungroup() %>%
  dplyr ::select(CASEID, b3_n, B3) %>%
  pivot_wider(names_from = b3_n, values_from = B3)

# Unir la base de mujeres con la de embarazos ancha
base_total <- base_MEF %>%
  left_join(base_emb_anch, by = "CASEID") %>% rename(dame = UBIGEO) %>% rename(dam = V101)

# Convertir nombres a minúsculas para estandarizar
names(base_total) <- tolower(names(base_total))


base_total <- base_total %>% mutate(
  v021_0 = as.integer(factor(v021)),
  dame = str_pad(
    string = dame, 
    width = 6,           
    side = "left",      
    pad = "0"           
  ),
  provincia = str_sub(dame, start = 1, end = 4))
# --------------------------------------#
#                ASFR                    #
# --------------------------------------#

options(survey.lonely.psu = "adjust")

# Total 

fert(base_total, Indicator="asfr", JK="Yes", Cluster = "v021_0")

#dam
dam <- unique(base_total$dam)
res  <- vector("list", length(dam))
names(res) <- dam

for (i in seq_along(dam)) {
  a <- dam[i]
  
  data <- base_total %>%
    filter(dam == a) %>%
    #group_by(v022) %>%
    mutate(v021_dam = as.integer(factor(v021))) %>%
    ungroup()

  
  out <- fert(
    Data.Name = data,
    Indicator = "asfr",
    Period    = 36,
    JK        = "Yes",
    Cluster = "v021_dam"
  )
  
  df <- as.data.frame(out) %>%
    filter(startsWith(AGE, "15-19")) %>% 
    mutate(dam = a)
  
  res[[i]] <- df       
}


estimacion_dam <- dplyr::bind_rows(res)

saveRDS(
  estimacion_dam,
  "Modelo_area/PER_2024/ASFR/15-19/output/estimación_directa_dam.rds"
)

# Provincia ---------------------------------------------------

# primero excluimos 

upm_estratos <- base_total %>%
  group_by(provincia) %>%
  summarise(
    n_upm = n_distinct(v021, na.rm = TRUE),
    .groups = "drop"
  )

prov_validos <- upm_estratos %>%
  filter(n_upm > 2) %>%
  pull(provincia)

# Ajustar la base
base_filtrada <- base_total %>%
  filter(provincia %in% prov_validos)


provincia <- unique(base_filtrada$provincia)
res  <- vector("list", length(provincia))
names(res) <- provincia

for (i in seq_along(provincia)) {
  a <- provincia[i]
  
  data <- base_filtrada %>%
    filter(provincia == a) %>%
    mutate(v021_prov = as.integer(factor(v021))) %>%
    ungroup()
  
  
  out <- fert(
    Data.Name = data,
    Indicator = "asfr",
    Period    = 36,
    JK        = "Yes",
    Cluster = "v021_prov"
  )
  
  
  df <- as.data.frame(out) %>%
    filter(startsWith(AGE, "15-19")) %>% 
    mutate(provincia = a)
  
  res[[i]] <- df           # guarda en la lista
}


estimacion_prov <- dplyr::bind_rows(res) %>%
  mutate(dam = case_when(
    nchar(as.character(provincia)) == 4 ~ substr(as.character(provincia), 1, 2),
    TRUE ~ NA_character_
  )) %>% mutate(dam = as.integer(dam))


# Número de UPMS
nupm <- base_MEF %>% distinct(V101, provincia, V021)  %>% rename(dam = V101) %>%
  group_by(dam, provincia) %>%
  tally() %>% rename(n_upm = n)

n_strata <- base_MEF %>% distinct(V101, provincia, V022)  %>% rename(dam = V101) %>%
  group_by(dam, provincia) %>%
  tally() %>% rename(n_strata = n)


nd <- base_MEF  %>% rename(dam = V101) %>%
  group_by(dam, provincia) %>%
  summarise(nd = n(), .groups = "drop")

estimacion_prov <- estimacion_prov %>% left_join(
  nupm,  by = c("dam", "provincia")) %>% left_join(
    nd, by = c("dam", "provincia"))%>% left_join(
      n_strata, by = c("dam", "provincia")) %>% mutate(gl = n_upm - n_strata) 

#CRITERIOS DE CALIDAD------------------------------------
##Excluyendo registros por falta de calidad
#base datos excluyendo los registros que cumplen criterios de calidad para 
#modelo sae

# 
base_sae <- estimacion_prov %>% data.frame()%>%
  filter(gl >= 2, nd > 30) %>%
  transmute(
    dam = dam,              # Id para los departamento
    provi = provincia,              #Id para los provincias
    edad = AGE,
    nd = nd,                # Número de observaciones por dominios
    ASFR = ASFR,      # Estimación de la variable
    vardir = SE ^ 2,      # Estimación de la varianza directa 
    cv = SE/ASFR,   
    deff = DEFT,
    n_upm = n_upm # Numero de upm
  )



saveRDS(base_sae, "Modelo_area/PER_2024/ASFR/15-19/output/estimación_directa_phv.rds")

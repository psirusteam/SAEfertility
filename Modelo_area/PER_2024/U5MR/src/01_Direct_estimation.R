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

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_MEF <- read.csv("Modelo_area/PER_2024/U5MR/input/REC0111_2024.csv") # Modulo mujeres en edad fertil 12 - 49 años
base_MEF <- base_MEF %>% filter(!is.na(V101)) %>% mutate(
  UBIGEO = str_pad(
    string = UBIGEO, 
    width = 6,           
    side = "left",      
    pad = "0"           
  ),
  provincia = str_sub(UBIGEO, start = 1, end = 4))

base_emb <- read.csv("Modelo_area/PER_2024/U5MR/input/REC21_2024.csv") #Historial de embarazos


################################################################################
###----------------------------- Direct estimation --------------------------###
################################################################################


# Unir la base de mujeres
base_total <- left_join(base_emb,
                        base_MEF %>% dplyr::select("V008", "V021", "V022", "V005", "CASEID", "V101","UBIGEO"),
                        by = "CASEID") %>% rename(dame = UBIGEO) %>% rename(dam = V101)

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
#                U5MR                    #
# --------------------------------------#

options(survey.lonely.psu = "adjust")

# Total 

chmort(base_total,  JK="Yes", Cluster = "v021_0")

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

  
  out <- chmort(
    Data.Name = data,
    JK        = "Yes",
    Cluster = "v021_dam"
  ) 
  
  df <- as.data.frame(out) %>%
    rownames_to_column("indicador") %>%
    filter(startsWith(indicador, "U5MR")) %>%   # solo U5MR
    mutate(dam = a)
  
  res[[i]] <- df           # guarda en la lista
}


estimacion_dam <- dplyr::bind_rows(res)

saveRDS(
  estimacion_dam,
  "Modelo_area/PER_2024/U5MR/output/estimación_directa_dam.rds"
)

#provincia

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
  
  
  out <- chmort(
    Data.Name = data,
    JK        = "Yes",
    Cluster = "v021_prov"
  )
  
  df <- as.data.frame(out) %>%
    rownames_to_column("indicador") %>%
    filter(startsWith(indicador, "U5MR")) %>%   # solo U5MR
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




#dame

# primero excluimos 

upm_estratos <- base_total %>%
  group_by(dame) %>%
  summarise(
    n_upm = n_distinct(v021, na.rm = TRUE),
    .groups = "drop"
  )

distritos_validos <- upm_estratos %>%
  filter(n_upm > 2) %>%
  pull(dame)

# Ajustar la base
base_filtrada <- base_total %>%
  filter(dame %in% distritos_validos)


dame <- unique(base_filtrada$dame)
res  <- vector("list", length(dame))
names(res) <- dame

for (i in seq_along(dame)) {
  a <- dame[i]
  
  data <- base_filtrada %>%
    filter(dame == a) %>%
    mutate(v021_dame = as.integer(factor(v021))) %>%
    ungroup()
  
  
  out <- chmort(
    Data.Name = data,
    JK        = "Yes",
    Cluster = "v021_dame"
  )
  
  df <- as.data.frame(out) %>%
    rownames_to_column("indicador") %>%
    filter(startsWith(indicador, "U5MR")) %>%   # solo U5MR
    mutate(dame = a)
  
  res[[i]] <- df           # guarda en la lista
}


estimacion_dame <- dplyr::bind_rows(res) %>%
  mutate(dam = case_when(
    nchar(as.character(dame)) == 6 ~ substr(as.character(dame), 1, 2),
    nchar(as.character(dame)) == 5 ~ substr(as.character(dame), 1, 1),
    TRUE ~ NA_character_
  )) %>% mutate(dam = as.integer(dam))
  

# Número de UPMS
nupm <- base_MEF %>% distinct(V101, UBIGEO, V021) %>%
  rename(dame = UBIGEO) %>% rename(dam = V101) %>%
  group_by(dam, dame) %>%
  tally() %>% rename(n_upm = n)

n_strata <- base_MEF %>% distinct(V101, UBIGEO, V022) %>%
  rename(dame = UBIGEO) %>% rename(dam = V101) %>%
  group_by(dam, dame) %>%
  tally() %>% rename(n_strata = n)


nd <- base_MEF %>% 
  rename(dame = UBIGEO) %>% rename(dam = V101) %>%
  group_by(dam, dame) %>%
  summarise(nd = n(), .groups = "drop")

estimacion <- estimacion_dame %>% left_join(
  nupm,  by = c("dam", "dame")) %>% left_join(
  nd, by = c("dam", "dame"))%>% left_join(
    n_strata, by = c("dam", "dame")) %>% mutate(gl = n_upm - n_strata) 



#CRITERIOS DE CALIDAD------------------------------------
##Excluyendo registros por falta de calidad
#base datos excluyendo los registros que cumplen criterios de calidad para 
#modelo sae



base_sae <- estimacion %>% data.frame()%>%
  filter(gl >= 2, nd > 30) %>%
  transmute(
    dam = dam,              # Id para los departamento
    dame = dame,              #Id para los distritos
    nd = nd,                # Número de observaciones por dominios
    U5MR = R,              # Estimación de la variable
    vardir = SE ^ 2,      # Estimación de la varianza directa
    cv = SE/U5MR,
    deff = DEFT,
    n_upm = n_upm # Numero de upm
  )

base_sae <- estimacion_prov %>% data.frame()%>%
  filter(gl >= 2, nd > 30) %>%
  transmute(
    dam = dam,              # Id para los departamento
    provincia = provincia,              #Id para los distritos
    nd = nd,                # Número de observaciones por dominios
    U5MR = R,              # Estimación de la variable
    vardir = SE ^ 2,      # Estimación de la varianza directa 
    cv = SE/U5MR,   
    deff = DEFT,
    n_upm = n_upm # Numero de upm
  )

saveRDS(base_sae, "Modelo_area/PER_2024/U5MR/output/estimación_directa_distritos_U5MR.rds")

#################################################
#             Proyecto : SAEfertility           #
#                   Redatam                     #
#################################################

rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(purrr)
library(dplyr)
library(Rcpp)
library(RcppProgress)
library(redatam)

peru <- redatam.open("Modelo_area/PER_2024/ASFR/35-39/input/cpv-per-2017-cde_diccionario.dicx")

redatam.entities(peru)
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "PERSONA")
redatam.variables(peru, "HOGAR")

CONTEOS <- redatam.query(peru, "freq PROVINCI.REDCODE 
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02",
                         tot.omit = FALSE)

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter(if_all(matches("label"), ~ .x != "__tot__" & .x != "na" & .x != "mv_" ))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

muj_35_39 <- CONTEOS2 %>%
  transmute(provi = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  dam = str_sub(provi, start = 1,end = 2),    
  sexo = as.character(C5P023_value),
  edad = C5P0412_value,
  value)%>%
  filter(edad >= 35 & edad <= 39 & sexo == 2) %>% group_by(dam,provi)%>%
  summarise(n = sum(value), .groups = "drop")

muj_total <- CONTEOS2 %>%
  transmute(provi = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  dam = str_sub(provi, start = 1,end = 2),    
  sexo = as.character(C5P023_value),
  edad = C5P0412_value,
  value)%>%
  filter( sexo == 2) %>% group_by(dam,provi)%>%
  summarise(n = sum(value), .groups = "drop")


# Suma del total mujeres de 15 a 49 años
sum(muj_35_39$n)
sum(muj_total$n)

saveRDS(muj_35_39, "Modelo_area/PER_2024/ASFR/35-39/output/censo_muj_35_39.rds")
saveRDS(muj_total, "Modelo_area/PER_2024/ASFR/35-39/output/censo_muj_total.rds")


## Promedio hijos nacidos vivos por dame ------------------------------------------

CONTEOS <- redatam.query(peru, "freq PROVINCI.REDCODE  by PERSONA.C5P02
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02
                                  by PERSONA.C5P27",
                         tot.omit = FALSE)

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter(if_all(matches("label"), ~ .x != "__tot__" & .x != "__na__" & .x != "__mv__" ))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

hijos_nacidos <- CONTEOS2 %>%
  transmute(provi = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  dam = str_sub(provi, start = 1,end = 2),   
  sexo = C5P022_value,
  edad = C5P0413_value,
  hijos_nacidos = C5P275_value,
  value)%>%
  filter(edad >= 15 & edad <= 49 & sexo == 2) %>% group_by(dam,provi)%>%
  summarise(hijos_nacidos = sum(value*hijos_nacidos), .groups = "drop")


saveRDS(
  hijos_nacidos,
  "Modelo_area/PER_2024/ASFR/35-39//output/censo_hijos_nacidos.rds"
)


#################### Variables Censo ##########################################

peru <- redatam.open("Modelo_area/PER_2024/ASFR/35-39/input/cpv-per-2017-cde_diccionario.dicx")


redatam.entities(peru)
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "PERSONA")

CONTEOS <- redatam.query(peru, "freq PROVINCI.REDCODE
                                  by VIVIENDA.VAREA
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02
                                  by PERSONA.ANEST
                                  by PERSONA.P09DISC
                                  by PERSONA.PBLOPER",
                         tot.omit = FALSE)
#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


CONTEOS2 %>% group_by(ANEST5_label, ANEST5_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER7_label, PBLOPER7_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()


censo_mrp <- CONTEOS2 %>%
  transmute(provi = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(C5P024_value),
  
  edad = case_when(
    C5P0413_value  %in% 0:14 ~ "1", # 5 a 14
    C5P0413_value  %in% 15:29 ~ "2", # 15 a 29
    C5P0413_value  %in% 30:44 ~ "3", # 30 a 44
    C5P0413_value  %in% 45:64 ~ "4", # 45 a 64
    TRUE ~ "5"), # 65 o mas
  
  anoest = case_when(
    C5P0413_value < 4| is.na(ANEST5_value) ~ "98",     # No aplica
    ANEST5_value == 99 ~ "99", #NS/NR
    ANEST5_value %in% 0 ~ "1",  # Sin educacion
    ANEST5_value %in% c(1:6) ~ "2",  # 1-6
    ANEST5_value %in% c(7:11) ~ "3",  # 7-12 (caso particular  de perú)
    ANEST5_value > 11 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),
  etnia = case_when(
    PBLOPER7_value == 1 ~ "1", # Indigena
    PBLOPER7_value == 2 ~ "2", # Afro
    TRUE ~ "3"), # Otro
  
  discapacidad = case_when(
    P09DISC6_value == 63 ~ "0", # No discapacitado
    TRUE ~ "1"), # Discapacitado
  value) %>%
  group_by(provi, area, sexo, edad, etnia, discapacidad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")


sum(censo_mrp$n)# Suma del total nacional

# agregados por nuevas variables
map(c(
  "provi",
  "area",
  "discapacidad",
  "sexo",
  "edad",
  "etnia",
  "anoest"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

## Variables agregadas.
#prop.table(table(censo_mrp$depto,censo_mrp$etnia),margin = 1)

tasa_censo <- model.matrix(provi ~ -1 +.,
                           data = censo_mrp %>% dplyr :: select(-n)) %>% 
  data.frame() %>%
  mutate(provi= censo_mrp$provi,
         n = censo_mrp$n) %>% 
  group_by( provi) %>%
  summarise_all(~weighted.mean(x = .,w = n)) %>%
  mutate(etnia1 = 1-etnia3-etnia2) %>% 
  dplyr:: select(-anoest98,-etnia3,-n) 

censo_mrp_m <- censo_mrp %>% filter(sexo == "2")  %>% dplyr :: select(- sexo)
tasa_censo_m <- model.matrix(provi ~ -1 +.,
                             data = censo_mrp_m  %>% dplyr :: select(-n)) %>% 
  data.frame() %>%
  mutate(provi = censo_mrp_m$provi,
         n = censo_mrp_m$n) %>% 
  group_by(provi) %>%
  summarise_all(~weighted.mean(x = .,w = n)) %>%
  mutate(etnia1 = 1-etnia3-etnia2) %>% 
  dplyr:: select(-anoest98,-etnia3,-n) 


# Alcantarillado  -----------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by VIVIENDA.VAREA
                          by VIVIENDA.C2P10 by PERSONA.C5P02",
                         tot.omit = FALSE)

ALCANTARILLADO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_alcantarillado <- ALCANTARILLADO %>%
  mutate(Pobx = ifelse(!C2P103_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_alcantarillado = Pobx/PobT) 

tasa_alcantarillado_m <- ALCANTARILLADO %>% filter(C5P024_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P103_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_alcantarillado_m = Pobx/PobT) 
# carencia de sanitario   -----------------------------------------------------------
# Energía eléctrica ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P11",
                         tot.omit = FALSE)

ELECTRICIDAD_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_electricidad <- ELECTRICIDAD_RED %>%
  mutate(Pobx = ifelse(!C2P114_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_electricidad = Pobx/PobT) 

tasa_electricidad_m <- ELECTRICIDAD_RED %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P114_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_electricidad_m = Pobx/PobT) 

# Agua cocinar ---------------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P06",
                         tot.omit = FALSE)

ACUEDUCTO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_agua <- ACUEDUCTO %>%
  mutate(Pobx = ifelse(!C2P064_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    ),
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_acueducto = Pobx/PobT) 

tasa_agua_m <- ACUEDUCTO %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P064_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    ),
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_acueducto_m = Pobx/PobT) 
# Gas natural ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by HOGAR.C3P12",
                         tot.omit = FALSE)

GAS_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_gas <- GAS_RED %>%
  mutate(Pobx = ifelse(!C3P124_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_gas = Pobx/PobT) 

tasa_gas_m <- GAS_RED %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C3P124_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_gas_m = Pobx/PobT) 
# Eliminación de basura ----------------------------------------------------
# Tasa de acceso a Internet  ----------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by HOGAR.C3P213",
                         tot.omit = FALSE)

INTERNET <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_internet <- INTERNET %>%
  mutate(Pobx = ifelse(C3P2134_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_internet = Pobx/PobT) 

tasa_internet_m <- INTERNET %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C3P2134_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_internet_m = Pobx/PobT) 
# Piso de tierra ----------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P05",
                         tot.omit = FALSE)

PISO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_piso <- PISO %>%
  mutate(Pobx = ifelse(C2P054_value %in% c(6), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            piso_tierra = Pobx/PobT) 

tasa_piso_m <- PISO %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C2P054_value %in% c(6), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            piso_tierra_m = Pobx/PobT) 
# Material de paredes ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P03",
                         tot.omit = FALSE)

PAREDES <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_paredes <- PAREDES %>%
  mutate(Pobx = ifelse(!C2P034_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_paredes = Pobx/PobT) 

tasa_paredes_m <- PAREDES %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P034_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_paredes_m = Pobx/PobT) 
# Material de techo ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P04",
                         tot.omit = FALSE)

TECHO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_techo <- TECHO %>%
  mutate(Pobx = ifelse(!C2P044_value %in% c(1:4), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_techo = Pobx/PobT) 

tasa_techo_m <- TECHO %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P044_value %in% c(1:4), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_techo_m = Pobx/PobT) 
# Tasa de personas con más 12 años de educación  y > 20 años. -------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                      by PERSONA.C5P041
                      by PERSONA.ANEST",
                         tot.omit = FALSE)
EDUCACION <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__" ) ))

tasa_edu_sup <- EDUCACION %>%
  mutate(Pobx = ifelse(C5P0414_value > 20 & ANEST5_value > 12,
                       value, 0),
         PobT = ifelse(C5P0414_value > 20, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            rezago_escolar = Pobx/PobT)

tasa_edu_sup_m <- EDUCACION %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C5P0414_value > 20 & ANEST5_value > 12,
                       value, 0),
         PobT = ifelse(C5P0414_value > 20, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            rezago_escolar_m = Pobx/PobT)

# Tasa de personas analfabeta. ------------------------------------------
# Población de 15 años y más que no sabe leer y escribir dividido por la 
# población de 15 años y más, multiplicado por 100.
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                      by PERSONA.C5P041
                      by PERSONA.C5P12",
                         tot.omit = FALSE)
ALFABETA <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__", "__na__") ))

tasa_analfabeta  <- ALFABETA %>%
  mutate(Pobx = ifelse(C5P0414_value > 15 & C5P125_value == 2,
                       value, 0),
         PobT = ifelse(C5P0414_value > 15, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            analfabeta = Pobx/PobT)

tasa_analfabeta_m  <- ALFABETA %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C5P0414_value > 15 & C5P125_value == 2,
                       value, 0),
         PobT = ifelse(C5P0414_value > 15, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            analfabeta_m = Pobx/PobT)



####################################################
OCUPACION <- redatam.query(peru, "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                           by PERSONA.PET
                           ", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))

group_by(OCUPACION2, PET4_label, PET4_value) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    ),
    ocupados = ifelse(PET4_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET4_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(provi, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion <- tabla %>%
  transmute(provi,
            tasa_desocupacion = ocupados0_1 / (ocupados0_1 + ocupados1_0)) %>% replace_na(list(tasa_desocupacion = 0))



OCUPACION2_m <- OCUPACION2 %>% filter(C5P022_value == "2") %>% 
  transmute(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    ),
    ocupados = ifelse(PET4_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET4_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(provi, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")


tabla <-
  pivot_wider(
    OCUPACION2_m,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion_m <- tabla %>%
  transmute(provi,
            tasa_desocupacion_m = ocupados0_1 / (ocupados0_1 + ocupados1_0)) %>% replace_na(list(tasa_desocupacion_m = 0))

############################# LAVADORA #############################
CONTEOS <- redatam.query(
  peru,
  "freq PROVINCI.REDCODE  by PERSONA.C5P02 by VIVIENDA.VAREA by HOGAR.C3P205",
  tot.omit = FALSE
)

LAVADORA<- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_LAVADORA<- LAVADORA%>% 
  mutate(Pobx = ifelse(!C3P2054_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
    
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_lavadora= Pobx/PobT)

tasa_LAVADORA_m<- LAVADORA %>% filter(C5P022_value == "2") %>%  
  mutate(Pobx = ifelse(!C3P2054_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
    
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_lavadora_m= Pobx/PobT)



############################# REFRIGERADORA #############################
CONTEOS <- redatam.query(
  peru,
  "freq PROVINCI.REDCODE  by PERSONA.C5P02 by VIVIENDA.VAREA by HOGAR.C3P204",
  tot.omit = FALSE
)

REFRIGERADORA<- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_refrigeradora <- REFRIGERADORA %>% 
  mutate(Pobx = ifelse(!C3P2044_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_refrigeradora= Pobx/PobT)

tasa_refrigeradora_m <- REFRIGERADORA %>% filter(C5P022_value == "2") %>%   
  mutate(Pobx = ifelse(!C3P2044_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_refrigeradora_m= Pobx/PobT)


############################# COMPUTADORA #############################
CONTEOS <- redatam.query(
  peru,
  "freq PROVINCI.REDCODE  by PERSONA.C5P02 by VIVIENDA.VAREA by HOGAR.C3P209",
  tot.omit = FALSE
)

COMPUTADORA<- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_computadora <- COMPUTADORA  %>% 
  mutate(Pobx = ifelse(!C3P2094_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_computadora= Pobx/PobT)

tasa_computadora_m <- COMPUTADORA  %>% filter(C5P022_value == "2") %>%   
  mutate(Pobx = ifelse(!C3P2094_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_computadora_m= Pobx/PobT)


############################# AUTOMOVIL #############################
CONTEOS <- redatam.query(
  peru,
  "freq PROVINCI.REDCODE  by PERSONA.C5P02 by VIVIENDA.VAREA by HOGAR.C3P214",
  tot.omit = FALSE
)

AUTOMOVIL<- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_automovil <- AUTOMOVIL%>% 
  mutate(Pobx = ifelse(!C3P2144_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_automovil= Pobx/PobT)

tasa_automovil_m <- AUTOMOVIL%>% filter(C5P022_value == "2") %>%  
  mutate(Pobx = ifelse(!C3P2144_value%in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_automovil_m= Pobx/PobT)

############################# LEÑA #############################
CONTEOS <- redatam.query(
  peru,
  "freq PROVINCI.REDCODE  by PERSONA.C5P02 by VIVIENDA.VAREA by HOGAR.C3P15",
  tot.omit = FALSE
)

LEÑA<- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_leña <- LEÑA %>% 
  mutate(Pobx = ifelse(C3P154_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            combustible_leña= Pobx/PobT)

tasa_leña_m <- LEÑA %>% filter(C5P022_value == "2") %>%  
  mutate(Pobx = ifelse(C3P154_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            combustible_leña_m= Pobx/PobT)

###################### VIVIENDA PROPIA ############################
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P13",
                         tot.omit = FALSE)

VIVIENDA_PROPIA <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_viv_propia <- VIVIENDA_PROPIA %>%
  mutate(Pobx = ifelse(!C2P134_value %in% c(2,3), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_vivienda_propia = Pobx/PobT) 

tasa_viv_propia_m <- VIVIENDA_PROPIA %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(!C2P134_value %in% c(2,3), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            carece_vivienda_propia_m = Pobx/PobT) 

###################### IDIOMA MATERNO CASTELLANO ############################
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by PERSONA.C5P11",
                         tot.omit = FALSE)

ESPAÑOL_MATERNO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_español_materno <- ESPAÑOL_MATERNO %>% 
  mutate(Pobx = ifelse(C5P114_value %in% c(10), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            español_materno = Pobx/PobT) 

tasa_español_materno_m <- ESPAÑOL_MATERNO %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C5P114_value %in% c(10), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            español_materno_m = Pobx/PobT) 
###################### TIPO VIV CASA ############################
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02  by VIVIENDA.VAREA
                          by VIVIENDA.C2P01",
                         tot.omit = FALSE)

CASA <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_casa <-CASA %>% 
  mutate(Pobx = ifelse(C2P014_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tipo_viv_casa = Pobx/PobT) 

tasa_casa_m <-CASA %>%  filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C2P014_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tipo_viv_casa_m = Pobx/PobT) 


# Tasa de personas empleadas -------------------------------------------

CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02 
                          by PERSONA.SITUEMP",
                         tot.omit = FALSE)
SITUEMPL <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_empleado <-SITUEMPL %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(3), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_empleado = Pobx/PobT) 

tasa_empleado_m <-SITUEMPL %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(3), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_empleado_m = Pobx/PobT) 

# Tasa de personas empleadoras -------------------------------------------

CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02 
                          by PERSONA.SITUEMP",
                         tot.omit = FALSE)
SITUEMPL <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_empleador <-SITUEMPL %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_empleador = Pobx/PobT) 

tasa_empleador_m <-SITUEMPL %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_empleador_m = Pobx/PobT) 

# Tasa de personas que trabajan por cuenta propia  ---------------------------

CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02 
                          by PERSONA.SITUEMP",
                         tot.omit = FALSE)
SITUEMPL <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_cuent_prop <-SITUEMPL %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_cuent_prop = Pobx/PobT) 

tasa_cuent_prop_m <-SITUEMPL %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(SITUEMP3_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_cuent_prop_m = Pobx/PobT) 

# Tasa de personas no afiliadas a ningun seguro  ---------------------------

CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02 
                          by PERSONA.C5P86",
                         tot.omit = FALSE)
SEGURO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

tasa_no_seguro <-SEGURO %>% 
  mutate(Pobx = ifelse(C5P863_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_no_seguro = Pobx/PobT) 

tasa_no_seguro_m <-SEGURO %>% filter(C5P022_value == "2") %>% 
  mutate(Pobx = ifelse(C5P863_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value ,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tasa_no_seguro_m = Pobx/PobT) 


# Promedio hijos nacidos vivos  ---------------------------

CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODE  by PERSONA.C5P02 
                          by PERSONA.C5P27  by PERSONA.C5P041",
                         tot.omit = FALSE)

hijos_nacidos <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__") ))

hijos_nacidos <- hijos_nacidos %>%
  transmute(provi = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  sexo = C5P022_value,
  edad = C5P0414_value,
  hijos_nacidos = C5P273_value,
  value)%>%
  filter(edad >= 15 & edad <= 49 & sexo == 2) %>% group_by(provi)%>%
  summarise(hijos_nacidos = sum(value*hijos_nacidos), .groups = "drop")

Prop_hijos <- hijos_nacidos %>% left_join(muj_15_49, by = "provi") %>% 
  mutate(mean_hijos = hijos_nacidos / n) %>% dplyr::select(-c("hijos_nacidos", "n", "dam"))

###################################################################


statelevel_predictors_df <- list(
  tasa_censo,
  tasa_alcantarillado,
  # tasa_sanitario,
  tasa_electricidad,
  tasa_agua,
  tasa_gas,
  # tasa_basuras,
  tasa_internet,
  tasa_piso,
  tasa_paredes,
  tasa_techo,
  tasa_edu_sup,
  tasa_analfabeta,
  tasa_desocupacion,
  tasa_automovil,
  tasa_casa,
  tasa_viv_propia,
  tasa_computadora,
  tasa_español_materno,
  tasa_LAVADORA,
  tasa_leña,
  tasa_refrigeradora,
  Prop_hijos,
  tasa_no_seguro,
  tasa_cuent_prop,
  tasa_empleador,
  tasa_empleado
) %>%
  reduce(.f = full_join) %>% 
  
  mutate_all(~ifelse(is.na(.),0, .))

saveRDS(statelevel_predictors_df, "Modelo_area/PER_2024/ASFR/35-39/input/statelevel_predictors_df_update_provi.rds")


statelevel_predictors_df_m <- list(
  tasa_censo_m,
  tasa_alcantarillado_m,
  # tasa_sanitario,
  tasa_electricidad_m,
  tasa_agua_m,
  tasa_gas_m,
  # tasa_basuras,
  tasa_internet_m,
  tasa_piso_m,
  tasa_paredes_m,
  tasa_techo_m,
  tasa_edu_sup_m,
  tasa_analfabeta_m,
  tasa_desocupacion_m,
  tasa_automovil_m,
  tasa_casa_m,
  tasa_viv_propia_m,
  tasa_computadora_m,
  tasa_español_materno_m,
  tasa_LAVADORA_m,
  tasa_leña_m,
  tasa_refrigeradora_m,
  Prop_hijos,
  tasa_no_seguro_m,
  tasa_cuent_prop_m,
  tasa_empleador_m,
  tasa_empleado_m
) %>%
  reduce(.f = full_join) %>% 
  
  mutate_all(~ifelse(is.na(.),0, .))

saveRDS(statelevel_predictors_df_m, "Modelo_area/PER_2024/ASFR/35-39/input/statelevel_predictors_df_update_provi_muj.rds")



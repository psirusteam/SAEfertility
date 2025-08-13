#################################################
#             Proyecto : SAEfertility           #
#   Estimacion directa usando la libraria DHS   #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
# install.packages("DHS.rates")
library(DHS.rates)
library(dplyr)
library(writexl)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

### Temporary directories ###
b_path <- getwd()

input <- file.path(b_path, "input")
output <- file.path(b_path, "output")
src <- file.path(b_path, "src")



base_MEF <- read.csv(file.path(input, "REC0111_2024.csv"), stringsAsFactors = FALSE)# Modulo mujeres en edad fertil 12 - 49 años
base_emb <- read.csv(file.path(input, "REC21_2024.csv"), stringsAsFactors = FALSE) #Historial de embarazos


# -------------------------------
# Preparar base TOTAL
# -------------------------------

# Convertir historial de embarazos a formato ancho (1 columna por nacimiento)
base_emb_anch <- base_emb %>%
  group_by(CASEID) %>%
  arrange(CASEID, B3) %>%
  mutate(b3_n = paste0("b3_", sprintf("%02d", row_number()))) %>%
  ungroup() %>%
  select(CASEID, b3_n, B3) %>%
  pivot_wider(names_from = b3_n, values_from = B3)

# Unir la base de mujeres con la de embarazos ancha
base_total <- base_MEF %>%
  left_join(base_emb_anch, by = "CASEID")

# Convertir nombres a minúsculas para estandarizar
names(base_total) <- tolower(names(base_total))

# Normalizar el factor de expansión (v005 viene por defecto en escala de millones)
base_total$v005 <- as.numeric(base_total$v005)/1000000

# -------------------------------
#  ASFR
# -------------------------------
options( survey.lonely.psu = "adjust" )
asfr_dhs <- fert(
  Data.Name = base_total,
  Indicator = "asfr",
  Period = 36,
  JK = "Yes"
)

write_xlsx(asfr_dhs, path = file.path(output, "asfr_dhs_36meses.xlsx"))

# -------------------------------
#  TFR
# -------------------------------
options( survey.lonely.psu = "adjust" )
tfr_dhs <- fert(
  Data.Name = base_total,
  Indicator = "tfr",
  Period = 36,
  JK = "Yes"
)

tfr_dhs <- as.data.frame(tfr_dhs)
write_xlsx(tfr_dhs, path = file.path(output, "tfr_dhs_36meses.xlsx"))

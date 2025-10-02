#################################################
#             Proyecto : SAEfertility           #
#       Función generalizada dela varianza      #
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
library(ggplot2)


################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

est_dir <- readRDS("Modelo_area/PER_2024/U5MR/output/estimación_directa_phv.rds")

# Transformación de la varianza ------------------------------------------------

est_dir_FGV<-est_dir %>%
  dplyr::select(dame,U5MR,nd,vardir, n_upm) %>%
  mutate(ln_sigma2=log(vardir))

#[5.2] Analisis grafico----

#U5MR versus el ln_sigma2
p1<-ggplot(est_dir_FGV,aes(x=U5MR,y=ln_sigma2))+
  geom_point()+geom_smooth(method = "loess")+
  xlab("U5MR por distrito")
#tamaño muestral versus ln_sigma2
p2<-ggplot((est_dir_FGV),aes(x=nd,y=ln_sigma2))+
  geom_point()+geom_smooth(method="loess")+
  xlab("Tamaño de la muestra por municipio")
p3 <- ggplot((est_dir_FGV),aes(x=n_upm,y=ln_sigma2))+
  geom_point()+geom_smooth(method="loess")+
  xlab("UPM por Varianza")
#Raiz cuadrada del promedio de U5MR
p4<-ggplot(est_dir_FGV, aes(x=sqrt(U5MR),y=ln_sigma2))+
  geom_point()+geom_smooth(method = "loess")+
  xlab("Raíz cuadrada U5MR por distrito")

p1
p2
p3
p4


# Modelo para la variancia------------------------------------------------------
#En este capitulo para el procedimiento de suavizamiento de la varianza 
#se probaron varios modelos que mejor desempeñon mostraran, tanto en la 
#significancia de los coeficientes como el la evaluacion global del model
# usando el coeficiente de determinacion R. Las diferentes combinaciones, 


FGV1<-lm(ln_sigma2 ~ log(nd), data = est_dir_FGV)
summary(FGV1)

## Obtener Valor de la constante Delta
delta.hat = sum(est_dir_FGV$vardir) / 
  sum(exp(fitted.values(FGV1))) 
delta.hat

##Obtener variancia suavizada
hat.sigma <- 
  data.frame(dame = est_dir_FGV$dame,
             hat_var = delta.hat * exp(fitted.values(FGV1)))

est_dir_FGV <- left_join(est_dir_FGV, hat.sigma)

##Validacion del modelo FGV----------------------------------------------------
#Validacion del modelo, buscar el supuesto de normalidad 
#con otras transformaciones
par(mfrow = c(2, 2))

plot(FGV1)

#Comparando variancia estimada versus pronosticada
ggplot(est_dir_FGV , 
       aes(y = vardir, x = hat_var)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  labs(x = "FGV", y = "VarDirEst") +
  ylab("Varianza del Estimador Directo")#no se publica

fgv_graf <- ggplot(est_dir_FGV %>%
                     arrange(nd), aes(x = 1:nrow(est_dir_FGV))) +
  geom_line(aes(y = vardir, color = "VarDirEst")) +
  geom_line(aes(y = hat_var, color = "FGV")) +
  labs(y = "Varianzas", x = "Tamaño muestral", color = " ") +
  scale_x_continuous(breaks = seq(1, nrow(est_dir_FGV), by = 10),
                     labels = est_dir_FGV$nd[order(est_dir_FGV$nd)][seq(1, 
                                                                        nrow(est_dir_FGV),by = 10)]) +
  scale_color_manual(values = c("FGV" = "Blue", "VarDirEst" = "Red"))

ggsave(plot = fgv_graf,
       filename =  "Modelo_area/PER_2024/U5MR/output/FGV_U5MR.jpeg", 
       scale = 3)


#Guardar la base con FGV  
saveRDS(est_dir_FGV, "Modelo_area/PER_2024/U5MR/output/estimación_directa_phv_FGV.rds")

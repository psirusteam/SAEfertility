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

est_dir <- readRDS("Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_phv.rds")

# Transformación de la varianza ------------------------------------------------

est_dir_FGV<-est_dir %>%
  select(dame,hijos_nacidos,nd,vardir, deff_muni) %>%
  mutate(ln_sigma2=log(vardir))

#[5.2] Analisis grafico----

#promedio de hijos vivos versus el ln_sigma2
p1<-ggplot(est_dir_FGV,aes(x=hijos_nacidos,y=ln_sigma2))+
  geom_point()+geom_smooth(method = "loess")+
  xlab("Promedio de hijos vivos por municipio")
#tamaño muestral versus ln_sigma2
p2<-ggplot((est_dir_FGV),aes(x=nd,y=ln_sigma2))+
  geom_point()+geom_smooth(method="loess")+
  xlab("Tamaño de la muestra por municipio")
#Raiz cuadrada del promedio de los hijos vivos
p4<-ggplot(est_dir_FGV, aes(x=sqrt(hijos_nacidos),y=ln_sigma2))+
  geom_point()+geom_smooth(method = "loess")+
  xlab("Raíz cuadrada del promedio de los hijos vivos por municipio")

p1
p2
p4


# Modelo para la variancia------------------------------------------------------
#En este capitulo para el procedimiento de suavizamiento de la varianza 
#se probaron varios modelos que mejor desempeñon mostraran, tanto en la 
#significancia de los coeficientes como el la evaluacion global del model
# usando el coeficiente de determinacion R. Las diferentes combinaciones, 


FGV1<-lm(ln_sigma2~1+I(nd^2)+I(sqrt(hijos_nacidos))+I(log(nd^hijos_nacidos)),
         data = est_dir_FGV)#modelo1
summary(FGV1)
FGV1<-lm(ln_sigma2~1+I(1/nd)+I(sqrt(hijos_nacidos))+I(log(nd^hijos_nacidos)),
         data = est_dir_FGV)#modelo2
summary(FGV1)
FGV1<-lm(ln_sigma2~1+I(1/sqrt(nd))+I(hijos_nacidos),
         data=est_dir_FGV)#modelo3
summary(FGV1)
FGV1<-lm(ln_sigma2~1+I(1/exp(nd))+I(hijos_nacidos**(1/3))+I((nd^hijos_nacidos)),
         data = est_dir_FGV)#modelo5
summary(FGV1)
FGV1<-lm(ln_sigma2~1+I(hijos_nacidos**(1/3))+I((nd^hijos_nacidos)),
         data = est_dir_FGV)#modelo6
summary(FGV1)
FGV1 <- lm(ln_sigma2 ~ nd + hijos_nacidos,
           data = est_dir_FGV)#modelo7 -final
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

ggplot(est_dir_FGV %>%
         arrange(nd), aes(x = 1:nrow(est_dir_FGV))) +
  geom_line(aes(y = vardir, color = "VarDirEst")) +
  geom_line(aes(y = hat_var, color = "FGV")) +
  labs(y = "Varianzas", x = "Tamaño muestral", color = " ") +
  scale_x_continuous(breaks = seq(1, nrow(est_dir_FGV), by = 10),
                     labels = est_dir_FGV$nd[order(est_dir_FGV$nd)][seq(1, 
                                                                  nrow(est_dir_FGV),by = 10)]) +
  scale_color_manual(values = c("FGV" = "Blue", "VarDirEst" = "Red"))


#Finalmente, se calcula la variable n_eff_FGV dividiendo nd
base_FH <- est_dir_FGV %>%
  mutate(
    deff_muni = ifelse(is.nan(deff_muni), 1,
                       deff_muni),
    deff_FGV = ifelse(
      vardir == 0 ,
      1,
      hat_var / (vardir / deff_muni)
    ),
    
    #Criterio MDS para regularizar el DeffFGV
    deff_FGV = ifelse(deff_FGV < 1, 1, deff_FGV),
    n_eff_FGV = nd / deff_FGV
  )

#Guardar la base con FGV  
saveRDS(base_FH, "Modelo_area/PER_2024/Promedio_hijos_nac_vivos/output/estimación_directa_phv_FGV.rds")

######PAISES CON MÁS PRUEBAS PT1.#####
#LIBRERIAS:
library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(EWSmethods)

#1.- FILTRADO DE DATOS:
#DATOS:
  #covid <- owid_covid() # de aqui se carga la base de datos de owid
str(covid)

#FILTRADO:
covid_china <- covid %>%filter(iso_code=="CHN")
#PARA GUARDAR LA BASE DE DATOS POR PAIS:
save(covid_china, file =  "03_out/data/Covid China.RData")

#UNIVARIADAS:
df_covid_china <- data.frame(
  tiem = seq(1, length(covid_china$date), 1) ,
  covid_china[ ,5:16]
)
names(df_covid_china)


#que siginifica cada columa:
# 3 -> new cases
# 4 -> new cases smoothed
#> ews univariate

#vector con las metricas:
ews_metrics <- c("SD","ar1","skew") 

################################################################################
names(df_covid_china[-c(1:10),c(1,3)]) #para que solo este el tiempo
#y los nuevos casos. Lo necesario para realizar el analisis.
#tiempo
#nuevos casos.

ews_univariado_china <- uniEWS(data = df_covid_china[-c(1:10),c(1,3)],
      metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
      method = "expanding", #VENTANA QUE SE EXPANDE
      burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
      threshold = 2, #VARIANZAS
      tail.direction = "one.tailed") 

plot(ews_univariado_china) 

 pdf("03_out/plots/ews_univariado_china.nuevos.casos.pdf", height = 8, width = 10)
 plot(ews_univariado_china)
 dev.off()

################################################################################
names(df_covid_china[-c(1:10),c(1,4)])
#timpo
#nuevos casos suavizados.

ews_univariado_china_2<- uniEWS(data = df_covid_china[-c(1:10),c(1,4)] ,
                           metrics =  ews_metrics,
                           method = "expanding",
                           burn_in = 10,
                           threshold = 2,
                           tail.direction = "one.tailed")
plot(ews_univariado_china_2)

 pdf("03_out/plots/ews_univariado_china.nuevos.suavizado.pdf", height = 8, width = 10)
 plot(ews_univariado_china_2)
 dev.off()

################################################################################
names(df_covid_china[-c(1:10),c(1,7)])
#tiempo
#nuevas muertes suavizadas

#tiene datos faltantes: 
  #library(dplyr)
  #library(zoo)
    
#funcion: DATOS NA:
head(df_covid_china[rowSums(is.na(df_covid_china)) == 0,]) ## para eliminar reglones con NA
df_covid_china<- df_covid_china[rowSums(is.na(df_covid_china)) == 0,]


ews_univariado_china_3<- uniEWS(data = df_covid_china [-c(1:10),c(1,7)] ,
                           metrics =  ews_metrics,
                          method = "expanding",
                         burn_in = 10,
                        threshold = 2,
                       tail.direction = "one.tailed")
plot(ews_univariado_china_3)

pdf("03_out/plots/ews_univariado_china.muertes.suavizado.pdf", height = 8, width = 10)
plot(ews_univariado_china_3)
dev.off()

####################################################################################
#intento multivariado 

#ews_multi_china <- multiEWS(data = df_covid_china, #QUE MEDIDAS TOMAR.
 #                              metrics =  c("meanAR","maxAR","meanSD","maxSD","eigenMAF",
  #                                          "mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV",
   #                                         "mutINFO"),
    #                           method = "expanding", #VENTANA QUE SE EXPANDE
     #                          burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
      #                         threshold = 2, #VARIANZAS
       #                        tail.direction = "one.tailed") 
#plot(ews_univariado_china) 

####################################################################################


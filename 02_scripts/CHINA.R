######PAISES CON MÁS PRUEBAS PT1.#####
#LIBRERIAS:
library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(EWSmethods)

#1.- FILTRADO DE DATOS:
#DATOS:
covid <- owid_covid() # de aqui se carga la base de datos de owid
str(covid)

#FILTRADO:
covid_china <- covid %>%filter(iso_code=="CHN")
#PARA GUARDAR LA BASE DE DATOS POR PAIS:
#save(objeto, "nombre del archivo.RData")

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

###########
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
# pdf("03_out/plots/ews_univariado_china.nuevos.casos.pdf", height = 8, width = 10)
# plot(ews_univariado_china)
# dev.off()

#############
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
# pdf("03_out/plots/ews_univariado_china.nuevos.suavizado.pdf", height = 8, width = 10)
# plot(ews_univariado_china_2)
# dev.off()

###############
names(df_covid_china[-c(1:10),c(1,7)])
#tiempo
#nuevas muertes suavizadas

#tiene datos faltantes: 
  #library(dplyr)
  #library(zoo)
    #Interpolate missing values
      #faltante <- df_covid_china[-c(1:10),c(1,7)] #<- na.approx(df$value)
        #faltante$new_deaths_smoothed_interpolated <- na.approx(faltante$new_deaths_smoothed)

datos_china_sin_na <- df_covid_china[!is.na(df_covid_china$new_deaths_smoothed), ]

#funcion:
head(covid_usa[rowSums(is.na(covid_usa)) == 0,]) ## para eliminar reglones con NA

ews_univariado_china_3<- uniEWS(data = datos_china_sin_na [-c(1:10),c(1,7)] ,
                           metrics =  ews_metrics,
                          method = "expanding",
                         burn_in = 10,
                        threshold = 2,
                       tail.direction = "one.tailed")
plot(ews_univariado_china_3)

#CON LOS DATOS INTENTO INTERPOLADOS:
#estimacion de los valores desconocidos, basados en otros.

#ews_univariado_china_4<- uniEWS(data = faltante[ ,c(1,3)], #datos interpolados
 #                               metrics =  ews_metrics,
  #                              method = "expanding",
   #                             burn_in = 10,
    #                            threshold = 2,
     #                           tail.direction = "one.tailed")
#plot(ews_univariado_china_4) 

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


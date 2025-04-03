##CAMPECHE
#CC----04
# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)

source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")
source("04_functions/incidencia.R")

#Procesamiento de los datos:
load("03_out/data/covid.mx.cc.2020.RData")
load("03_out/data/covid.mx.cc.2021.RData")
load("03_out/data/covid.mx.cc.2022.RData")
load("03_out/data/covid.mx.cc.2023.RData")
load("03_out/data/covid.mx.cc.2024.RData")

#2020
  #covid.mx.cc.2020$CLASIFICACION_FINAL
campeche_positivos <- filter(covid.mx.cc.2020, CLASIFICACION_FINAL ==1 |
                               CLASIFICACION_FINAL == 2|
                               CLASIFICACION_FINAL == 3)

avance(campeche_positivos)->campeche_incidencia_20

campeche_incidencia_20

campeche_ewsdatos_20 <- data.frame(
  time = seq(1, length(campeche_incidencia_20$FECHA_SINTOMAS), 1) ,
  casos = campeche_incidencia_20$positivos)

#ews_metrics <- c("SD","ar1","skew")

campeche_ews_20 <- uniEWS(data = campeche_ewsdatos_20,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(campeche_ews_20)
data.uni.ews(campeche_ews_20) ->ews_campeche_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_campeche_2020)-> ews_campeche_plot_2020

#OBJETO GGPLOT: 
ews_campeche_plot_2020 +labs(title = "CAMPECHE", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red")


#2021
campeche_positivos_21 <- filter(covid.mx.cc.2021, CLASIFICACION_FINAL ==1 |
                               CLASIFICACION_FINAL == 2|
                               CLASIFICACION_FINAL == 3)

avance(campeche_positivos_21)->campeche_incidencia_21

campeche_incidencia_21

campeche_ewsdatos_21 <- data.frame(
  time = seq(1, length(campeche_incidencia_21$FECHA_SINTOMAS), 1) ,
  casos = campeche_incidencia_21$positivos)

#ews_metrics <- c("SD","ar1","skew")

campeche_ews_21 <- uniEWS(data = campeche_ewsdatos_21,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
plot(campeche_ews_21)
data.uni.ews(campeche_ews_21) ->ews_campeche_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_campeche_2021)-> ews_campeche_plot_2021

#OBJETO GGPLOT: 
ews_campeche_plot_2021+labs(title = "CAMPECHE", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red")


#2022
  covid.mx.cc.2022$CLASIFICACION_FINAL

  campeche_positivos_22 <- filter(covid.mx.cc.2022, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(campeche_positivos_22)->campeche_incidencia_22

campeche_incidencia_22

campeche_ewsdatos_22 <- data.frame(
  time = seq(1, length(campeche_incidencia_22$FECHA_SINTOMAS), 1) ,
  casos = campeche_incidencia_22$positivos)

#ews_metrics <- c("SD","ar1","skew")

campeche_ews_22 <- uniEWS(data = campeche_ewsdatos_22,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
plot(campeche_ews_22)
data.uni.ews(campeche_ews_22) ->ews_campeche_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_campeche_2022)-> ews_campeche_plot_2022

#OBJETO GGPLOT: 
ews_campeche_plot_2022+labs(title = "CAMPECHE", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red")


#2023
covid.mx.cc.2023$CLASIFICACION_FINAL

campeche_positivos_23 <- filter(covid.mx.cc.2023, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(campeche_positivos_23)->campeche_incidencia_23

campeche_incidencia_23

campeche_ewsdatos_23 <- data.frame(
  time = seq(1, length(campeche_incidencia_23$FECHA_SINTOMAS), 1) ,
  casos = campeche_incidencia_23$positivos)

#ews_metrics <- c("SD","ar1","skew")

campeche_ews_23 <- uniEWS(data = campeche_ewsdatos_23,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
plot(campeche_ews_23)
data.uni.ews(campeche_ews_23) ->ews_campeche_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_campeche_2023)-> ews_campeche_plot_2023

#OBJETO GGPLOT: 
ews_campeche_plot_2023+labs(title = "CAMPECHE", subtitle = "2023")+
  geom_hline(yintercept = c(2), col = "red")


#2024-5
covid.mx.cc.2024$CLASIFICACION_FINAL_COVID

campeche_positivos_24 <- filter(covid.mx.cc.2024, CLASIFICACION_FINAL_COVID ==1 |
                                  CLASIFICACION_FINAL_COVID == 2|
                                  CLASIFICACION_FINAL_COVID == 3)

avance(campeche_positivos_24)->campeche_incidencia_24

campeche_incidencia_24

campeche_ewsdatos_24 <- data.frame(
  time = seq(1, length(campeche_incidencia_24$FECHA_SINTOMAS), 1) ,
  casos = campeche_incidencia_24$positivos)

#ews_metrics <- c("SD","ar1","skew")

campeche_ews_24 <- uniEWS(data = campeche_ewsdatos_24,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
plot(campeche_ews_24)
data.uni.ews(campeche_ews_24) ->ews_campeche_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_campeche_2024)-> ews_campeche_plot_2024

#OBJETO GGPLOT: 
ews_campeche_plot_2024+labs(title = "CAMPECHE", subtitle = "2024")+
  geom_hline(yintercept = c(2), col = "red")


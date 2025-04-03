###YUCATAN###
#31
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
################################################################################
load("03_out/data/covid.yucatan.2020.RData")
load("03_out/data/covid.yucatan.2021.RData")
load("03_out/data/covid.yucatan.2022.RData")
load("03_out/data/covid.yucatan.2023.RData")
load("03_out/data/covid.yucatan.2024.RData")
################################################################################
#2020
yucatan_positivos <- filter(covid.yucatan.2020, CLASIFICACION_FINAL ==1 |
                               CLASIFICACION_FINAL == 2|
                               CLASIFICACION_FINAL == 3)

avance(yucatan_positivos)-> yucatan_incidencia_20
yucatan_incidencia_20

yucatan_ewsdatos_20 <- data.frame(
  time = seq(1, length(yucatan_incidencia_20$FECHA_SINTOMAS), 1) ,
  casos = yucatan_incidencia_20$positivos)

#ews_metrics <- c("SD","ar1","skew")

yucatan_ews_20 <- uniEWS(data = yucatan_ewsdatos_20,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
plot(yucatan_ews_20)
data.uni.ews(yucatan_ews_20) ->ews_yucatan_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_yucatan_2020)-> ews_yucatan_plot_2020

#OBJETO GGPLOT: 
ews_yucatan_plot_2020 +labs(title = "YUCATAN", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red")

################################################################################
#2021
yucatan_positivos_21 <- filter(covid.yucatan.2021, CLASIFICACION_FINAL ==1 |
                              CLASIFICACION_FINAL == 2|
                              CLASIFICACION_FINAL == 3)

avance(yucatan_positivos_21)-> yucatan_incidencia_21
yucatan_incidencia_21

yucatan_ewsdatos_21 <- data.frame(
  time = seq(1, length(yucatan_incidencia_21$FECHA_SINTOMAS), 1) ,
  casos = yucatan_incidencia_21$positivos)

#ews_metrics <- c("SD","ar1","skew")

yucatan_ews_21 <- uniEWS(data = yucatan_ewsdatos_21,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
)
plot(yucatan_ews_21)
data.uni.ews(yucatan_ews_21) ->ews_yucatan_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_yucatan_2021)-> ews_yucatan_plot_2021

#OBJETO GGPLOT: 
ews_yucatan_plot_2021 +labs(title = "YUCATAN", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red")

################################################################################
#2022
  #covid.yucatan.2022$CLASIFICACION_FINAL
yucatan_positivos_22 <- filter(covid.yucatan.2022, CLASIFICACION_FINAL ==1 |
                                 CLASIFICACION_FINAL == 2|
                                 CLASIFICACION_FINAL == 3)

avance(yucatan_positivos_22)-> yucatan_incidencia_22
yucatan_incidencia_22

yucatan_ewsdatos_22 <- data.frame(
  time = seq(1, length(yucatan_incidencia_22$FECHA_SINTOMAS), 1) ,
  casos = yucatan_incidencia_22$positivos)

#ews_metrics <- c("SD","ar1","skew")

yucatan_ews_22 <- uniEWS(data = yucatan_ewsdatos_22,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
)
plot(yucatan_ews_22)
data.uni.ews(yucatan_ews_22) ->ews_yucatan_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_yucatan_2022)-> ews_yucatan_plot_2022

#OBJETO GGPLOT: 
ews_yucatan_plot_2022 +labs(title = "YUCATAN", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red")

################################################################################
#2023
  #covid.yucatan.2023
yucatan_positivos_23 <- filter(covid.yucatan.2023, CLASIFICACION_FINAL ==1 |
                                 CLASIFICACION_FINAL == 2|
                                 CLASIFICACION_FINAL == 3)

avance(yucatan_positivos_23)-> yucatan_incidencia_23
yucatan_incidencia_23

yucatan_ewsdatos_23<- data.frame(
  time = seq(1, length(yucatan_incidencia_23$FECHA_SINTOMAS), 1) ,
  casos = yucatan_incidencia_23$positivos)

#ews_metrics <- c("SD","ar1","skew")

yucatan_ews_23 <- uniEWS(data = yucatan_ewsdatos_23,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
)
plot(yucatan_ews_23)
data.uni.ews(yucatan_ews_23) ->ews_yucatan_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_yucatan_2023)-> ews_yucatan_plot_2023

#OBJETO GGPLOT: 
ews_yucatan_plot_2023 +labs(title = "YUCATAN", subtitle = "2023")+
  geom_hline(yintercept = c(2), col = "red")

################################################################################
#2024-5
  covid.yucatan.2024$CLASIFICACION_FINAL_COVID
yucatan_positivos_24 <- filter(covid.yucatan.2024, CLASIFICACION_FINAL_COVID ==1 |
                                 CLASIFICACION_FINAL_COVID == 2|
                                 CLASIFICACION_FINAL_COVID == 3)

avance(yucatan_positivos_24)-> yucatan_incidencia_24
yucatan_incidencia_24

yucatan_ewsdatos_24 <- data.frame(
  time = seq(1, length(yucatan_incidencia_24$FECHA_SINTOMAS), 1) ,
  casos = yucatan_incidencia_24$positivos)

#ews_metrics <- c("SD","ar1","skew")

yucatan_ews_24 <- uniEWS(data = yucatan_ewsdatos_24,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
)
plot(yucatan_ews_24)
data.uni.ews(yucatan_ews_24) ->ews_yucatan_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_yucatan_2024)-> ews_yucatan_plot_2024

#OBJETO GGPLOT: 
ews_yucatan_plot_2024 +labs(title = "YUCATAN", subtitle = "2024-5")+
  geom_hline(yintercept = c(2), col = "red")

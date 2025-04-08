###OAXACA###
##20	OAXACA	OC
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
load("03_out/data/covid_oaxaca_2020.RData")
load("03_out/data/covid_oaxaca_2021.RData")
load("03_out/data/covid_oaxaca_2022.RData")
load("03_out/data/covid_oaxaca_2023.RData")
load("03_out/data/covid_oaxaca_2024.RData")
################################################################################
#2020
  #covid_oaxaca_2020
oaxaca_positivos <- filter(covid_oaxaca_2020, CLASIFICACION_FINAL ==1 |
                              CLASIFICACION_FINAL == 2|
                              CLASIFICACION_FINAL == 3)

avance(oaxaca_positivos)-> oaxaca_incidencia_20
oaxaca_incidencia_20

oaxaca_ewsdatos_20 <- data.frame(
  time = seq(1, length(oaxaca_incidencia_20$FECHA_SINTOMAS), 1) ,
  casos = oaxaca_incidencia_20$positivos)

#ews_metrics <- c("SD","ar1","skew")

oaxaca_ews_20 <- uniEWS(data = oaxaca_ewsdatos_20,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
)
plot(oaxaca_ews_20)
data.uni.ews(oaxaca_ews_20) ->ews_oaxaca_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_oaxaca_2020)-> ews_oaxaca_plot_2020

#OBJETO GGPLOT: 
pdf("03_out/plots/oaxaca.20.1.pdf", height = 8, width = 10)
ews_oaxaca_plot_2020 +labs(title = "OAXACA", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()
################################################################################
#2021
  #covid_oaxaca_2021
oaxaca_positivos_21 <- filter(covid_oaxaca_2021, CLASIFICACION_FINAL ==1 |
                             CLASIFICACION_FINAL == 2|
                             CLASIFICACION_FINAL == 3)

avance(oaxaca_positivos_21)-> oaxaca_incidencia_21
oaxaca_incidencia_21

oaxaca_ewsdatos_21 <- data.frame(
  time = seq(1, length(oaxaca_incidencia_21$FECHA_SINTOMAS), 1) ,
  casos = oaxaca_incidencia_21$positivos)

#ews_metrics <- c("SD","ar1","skew")

oaxaca_ews_21 <- uniEWS(data = oaxaca_ewsdatos_21,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(oaxaca_ews_21)
data.uni.ews(oaxaca_ews_21) ->ews_oaxaca_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_oaxaca_2021)-> ews_oaxaca_plot_2021

#OBJETO GGPLOT: 
pdf("03_out/plots/oaxaca.22.1.pdf", height = 8, width = 10)
ews_oaxaca_plot_2021 +labs(title = "OAXACA", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()
################################################################################
#2022
  #covid_oaxaca_2022
oaxaca_positivos_22 <- filter(covid_oaxaca_2022, CLASIFICACION_FINAL ==1 |
                                CLASIFICACION_FINAL == 2|
                                CLASIFICACION_FINAL == 3)

avance(oaxaca_positivos_22)-> oaxaca_incidencia_22
oaxaca_incidencia_22

oaxaca_ewsdatos_22 <- data.frame(
  time = seq(1, length(oaxaca_incidencia_22$FECHA_SINTOMAS), 1) ,
  casos = oaxaca_incidencia_22$positivos)

#ews_metrics <- c("SD","ar1","skew")

oaxaca_ews_22 <- uniEWS(data = oaxaca_ewsdatos_22,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(oaxaca_ews_22)
data.uni.ews(oaxaca_ews_22) ->ews_oaxaca_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_oaxaca_2022)-> ews_oaxaca_plot_2022

#OBJETO GGPLOT: 
pdf("03_out/plots/oaxaca.22.1.pdf", height = 8, width = 10)
ews_oaxaca_plot_2022 +labs(title = "OAXACA", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()
################################################################################
#2023
  #covid_oaxaca_2023
oaxaca_positivos_23 <- filter(covid_oaxaca_2023, CLASIFICACION_FINAL ==1 |
                                CLASIFICACION_FINAL == 2|
                                CLASIFICACION_FINAL == 3)

avance(oaxaca_positivos_23)-> oaxaca_incidencia_23
oaxaca_incidencia_23

oaxaca_ewsdatos_23 <- data.frame(
  time = seq(1, length(oaxaca_incidencia_23$FECHA_SINTOMAS), 1) ,
  casos = oaxaca_incidencia_23$positivos)

#ews_metrics <- c("SD","ar1","skew")

oaxaca_ews_23 <- uniEWS(data = oaxaca_ewsdatos_23,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(oaxaca_ews_23)
data.uni.ews(oaxaca_ews_23) ->ews_oaxaca_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_oaxaca_2023)-> ews_oaxaca_plot_2023

#OBJETO GGPLOT: 
pdf("03_out/plots/oaxaca.23.1.pdf", height = 8, width = 10)
ews_oaxaca_plot_2023 +labs(title = "OAXACA", subtitle = "2023.NO señal")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()
################################################################################
#2024-5
  #covid_oaxaca_2024$CLASIFICACION_FINAL_COVID
oaxaca_positivos_24 <- filter(covid_oaxaca_2024, CLASIFICACION_FINAL_COVID ==1 |
                                CLASIFICACION_FINAL_COVID == 2|
                                CLASIFICACION_FINAL_COVID == 3)

avance(oaxaca_positivos_24)-> oaxaca_incidencia_24
oaxaca_incidencia_24

oaxaca_ewsdatos_24 <- data.frame(
  time = seq(1, length(oaxaca_incidencia_24$FECHA_SINTOMAS), 1) ,
  casos = oaxaca_incidencia_24$positivos)

#ews_metrics <- c("SD","ar1","skew")

oaxaca_ews_24 <- uniEWS(data = oaxaca_ewsdatos_24,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(oaxaca_ews_24)
data.uni.ews(oaxaca_ews_24) ->ews_oaxaca_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_oaxaca_2024)-> ews_oaxaca_plot_2024

#OBJETO GGPLOT: 
pdf("03_out/plots/oaxaca.24.1.pdf", height = 8, width = 10)
ews_oaxaca_plot_2024 +labs(title = "OAXACA", subtitle = "2024-5")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

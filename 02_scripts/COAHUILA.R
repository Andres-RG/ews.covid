#COAHUILA DE ZARAGOZA
#CL---05
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
load("03_out/data/covid.mx.cl.2020.RData")
load("03_out/data/covid.mx.cl.2021.RData")
load("03_out/data/covid.mx.cl.2022.RData")
load("03_out/data/covid.mx.cl.2023.RData")
load("03_out/data/covid.mx.cl.2024.RData")

#2020
#covid.mx.cl.2020$CLASIFICACION_FINAL
coahuila_positivos <- filter(covid.mx.cl.2020, CLASIFICACION_FINAL ==1 |
                               CLASIFICACION_FINAL == 2|
                               CLASIFICACION_FINAL == 3)

avance(coahuila_positivos)-> coahuila_incidencia_20
coahuila_incidencia_20

coahuila_ewsdatos_20 <- data.frame(
  time = seq(1, length(coahuila_incidencia_20$FECHA_SINTOMAS), 1) ,
  casos = coahuila_incidencia_20$positivos)

#ews_metrics <- c("SD","ar1","skew")

coahuila_ews_20 <- uniEWS(data = coahuila_ewsdatos_20,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
pdf("03_out/plots/coahuila.20.pdf", height = 8, width = 10)
plot(coahuila_ews_20)
dev.off()

data.uni.ews(coahuila_ews_20) ->ews_coahuila_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_coahuila_2020)-> ews_coahuila_plot_2020

#OBJETO GGPLOT: 
pdf("03_out/plots/coahuila.20.1.pdf", height = 8, width = 10)
ews_coahuila_plot_2020 +labs(title = "COAHUILA", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

#2021
#  covid.mx.cl.2021$CLASIFICACION_FINAL
coahuila_positivos_21 <- filter(covid.mx.cl.2021, CLASIFICACION_FINAL ==1 |
                               CLASIFICACION_FINAL == 2|
                               CLASIFICACION_FINAL == 3)

avance(coahuila_positivos_21)-> coahuila_incidencia_21
coahuila_incidencia_21

coahuila_ewsdatos_21 <- data.frame(
  time = seq(1, length(coahuila_incidencia_21$FECHA_SINTOMAS), 1) ,
  casos = coahuila_incidencia_21$positivos)

#ews_metrics <- c("SD","ar1","skew")

coahuila_ews_21 <- uniEWS(data = coahuila_ewsdatos_21,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
pdf("03_out/plots/coahuila.21.pdf", height = 8, width = 10)
plot(coahuila_ews_21)
dev.off()

data.uni.ews(coahuila_ews_21) ->ews_coahuila_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_coahuila_2021)-> ews_coahuila_plot_2021

#OBJETO GGPLOT: 
pdf("03_out/plots/coahuila.21.1.pdf", height = 8, width = 10)
ews_coahuila_plot_2021 +labs(title = "COAHUILA", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

#2022
  covid.mx.cl.2022$CLASIFICACION_FINAL
coahuila_positivos_22 <- filter(covid.mx.cl.2022, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(coahuila_positivos_22)-> coahuila_incidencia_22
coahuila_incidencia_22

coahuila_ewsdatos_22 <- data.frame(
  time = seq(1, length(coahuila_incidencia_22$FECHA_SINTOMAS), 1) ,
  casos = coahuila_incidencia_22$positivos)

#ews_metrics <- c("SD","ar1","skew")

coahuila_ews_22 <- uniEWS(data = coahuila_ewsdatos_22,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
pdf("03_out/plots/coahuila.22.pdf", height = 8, width = 10)
plot(coahuila_ews_22)
dev.off()

data.uni.ews(coahuila_ews_22) ->ews_coahuila_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_coahuila_2022)-> ews_coahuila_plot_2022

#OBJETO GGPLOT: 
pdf("03_out/plots/coahuila.22.2.pdf", height = 8, width = 10)
ews_coahuila_plot_2022 +labs(title = "COAHUILA", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()


#2023
covid.mx.cl.2023$CLASIFICACION_FINAL
coahuila_positivos_23 <- filter(covid.mx.cl.2023, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(coahuila_positivos_23)-> coahuila_incidencia_23
coahuila_incidencia_23

coahuila_ewsdatos_23 <- data.frame(
  time = seq(1, length(coahuila_incidencia_23$FECHA_SINTOMAS), 1) ,
  casos = coahuila_incidencia_23$positivos)

#ews_metrics <- c("SD","ar1","skew")

coahuila_ews_23 <- uniEWS(data = coahuila_ewsdatos_23,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
pdf("03_out/plots/coahuila.23.pdf", height = 8, width = 10)
plot(coahuila_ews_23)
dev.off()

data.uni.ews(coahuila_ews_23) ->ews_coahuila_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_coahuila_2023)-> ews_coahuila_plot_2023

#OBJETO GGPLOT: 
pdf("03_out/plots/coahuila.23.1.pdf", height = 8, width = 10)
ews_coahuila_plot_2023 +labs(title = "COAHUILA", subtitle = "2023")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

#2024-5
  #covid.mx.cl.2024$CLASIFICACION_FINAL_COVID
coahuila_positivos_24 <- filter(covid.mx.cl.2024, CLASIFICACION_FINAL_COVID ==1 |
                                  CLASIFICACION_FINAL_COVID == 2|
                                  CLASIFICACION_FINAL_COVID == 3)

avance(coahuila_positivos_24)-> coahuila_incidencia_24
coahuila_incidencia_24

coahuila_ewsdatos_24 <- data.frame(
  time = seq(1, length(coahuila_incidencia_24$FECHA_SINTOMAS), 1) ,
  casos = coahuila_incidencia_24$positivos)

#ews_metrics <- c("SD","ar1","skew")

coahuila_ews_24 <- uniEWS(data = coahuila_ewsdatos_24,
                          metrics = ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed"
)
pdf("03_out/plots/coahuila.24.pdf", height = 8, width = 10)
plot(coahuila_ews_24)
dev.off()
pdf("03_out/plots/coahuila.24.1.pdf", height = 8, width = 10)

data.uni.ews(coahuila_ews_24) ->ews_coahuila_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_coahuila_2024)-> ews_coahuila_plot_2024

#OBJETO GGPLOT: 
pdf("03_out/plots/coahuila.24.1.pdf", height = 8, width = 10)
ews_coahuila_plot_2024 +labs(title = "COAHUILA", subtitle = "2024-5")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

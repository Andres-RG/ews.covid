####29	TLAXCALA	TL###
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
source("04_functions/ewsdataobjeto.R")
################################################################################
load("03_out/data/covid_tlaxcala_2020.RData")
load("03_out/data/covid_tlaxcla_2021.RData")
load("03_out/data/covid_tlaxcla_2022.RData")
load("03_out/data/covid_tlaxcala_2023.RData")
load("03_out/data/covid_tlaxcala_2024.RData")
################################################################################
#2020
  #covid_tlaxcala_2020
tlaxcala_positivos_20 <- filter(covid_tlaxcala_2020, CLASIFICACION_FINAL ==1 |
                             CLASIFICACION_FINAL == 2|
                             CLASIFICACION_FINAL == 3)

avance(tlaxcala_positivos_20)-> tlaxcala_incidencia_20
tlaxcala_incidencia_20

ews_analisis(tlaxcala_incidencia_20) -> tlaxcala_2020

plot(tlaxcala_2020)
data.uni.ews(tlaxcala_2020) ->ews_tlaxcala_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_tlaxcala_2020)-> ews_tlaxcala_plot_2020

#OBJETO GGPLOT: 
ews_tlaxcala_plot_2020 +labs(title = "TLAXCALA", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red")


################################################################################
#2023
  #covid_tlaxcala_2023
tlaxcala_positivos_23 <- filter(covid_tlaxcala_2023, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(tlaxcala_positivos_23)-> tlaxcala_incidencia_23
tlaxcala_incidencia_23

ews_analisis(tlaxcala_incidencia_23) -> tlaxcala_2023

plot(tlaxcala_2023)
data.uni.ews(tlaxcala_2023) ->ews_tlaxcala_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_tlaxcala_2023)-> ews_tlaxcala_plot_2023

#OBJETO GGPLOT: 
ews_tlaxcala_plot_2023 +labs(title = "TLAXCALA", subtitle = "2023")+
  geom_hline(yintercept = c(2), col = "red")
################################################################################
#2022
  #covid_tlaxcla_2022
tlaxcala_positivos_22 <- filter(covid_tlaxcla_2022, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(tlaxcala_positivos_22)-> incidencia_tlaxcala_22


ews_analisis(incidencia_tlaxcala_22)-> ews_tlaxcala_22
plot(ews_tlaxcala_22)
data.uni.ews(ews_tlaxcala_22) ->ews_tlaxcala_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_tlaxcala_2022)-> ews_tlaxcala_plot_2022

#OBJETO GGPLOT: 
ews_tlaxcala_plot_2022 +labs(title = "TLAXCALA", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red")


################################################################################
#2021
  #covid_tlaxcla_2021
tlaxcala_positivos_21 <- filter(covid_tlaxcla_2021, CLASIFICACION_FINAL ==1 |
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

avance(tlaxcala_positivos_21)-> incidencia_tlaxcala_21
incidencia_tlaxcala_21

ews_analisis(incidencia_tlaxcala_21)-> ews_tlaxcala_21

plot(ews_tlaxcala_21)

data.uni.ews(ews_tlaxcala_21) ->ews_tlaxcala_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_tlaxcala_2021)-> ews_tlaxcala_plot_2021

#OBJETO GGPLOT: 
ews_tlaxcala_plot_2021 +labs(title = "TLAXCALA", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red")

################################################################################
#2024-5
  covid_tlaxcala_2024$CLASIFICACION_FINAL_COVID

tlaxcala_positivos_24 <- filter(covid_tlaxcala_2024, CLASIFICACION_FINAL_COVID ==1 |
                                  CLASIFICACION_FINAL_COVID == 2|
                                  CLASIFICACION_FINAL_COVID == 3)

avance(tlaxcala_positivos_24)-> incidencia_tlaxcala_24
incidencia_tlaxcala_24

ews_analisis(incidencia_tlaxcala_24)-> ews_tlaxcala_24
plot(ews_tlaxcala_24)

data.uni.ews(ews_tlaxcala_24) ->ews_tlaxcala_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_tlaxcala_2024)-> ews_tlaxcala_plot_2024

#OBJETO GGPLOT: 
ews_tlaxcala_plot_2024 +labs(title = "TLAXCALA", subtitle = "2024")+
  geom_hline(yintercept = c(2), col = "red")

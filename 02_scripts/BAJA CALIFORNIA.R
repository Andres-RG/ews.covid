###BAJA CALIFORNIA###
# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)

#Procesamiento de los datos:
################################################################################
#2020.
load("03_out/data/covid.mx.bc.2020.RData")
covid.mx.bc.2020$CLASIFICACION_FINAL
casos_positivos_bc_2020 <- filter(covid.mx.bc.2020, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2020 =================================================
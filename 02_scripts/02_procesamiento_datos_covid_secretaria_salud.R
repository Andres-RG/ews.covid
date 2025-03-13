# procesamiento de datos de covid Mexio
##-----------------------------------------------
# se cargan las librerias
library(tidyr)
library(tidyverse)
library(dplyr)
##-----------------------------------------------
# se cargan los datos
#2024-5
covid.mx.ss <- read.csv("01_raw_data/COVID19MEXICO.csv") # covid mexico secretaria de salud
str(covid.mx.ss)

#2020
covid.mx.ss.2020 <- read.csv("01_raw_data/COVID19MEXICO2020.csv")
str(covid.mx.ss.2020)

#2021

#2022

#2023

#2024
##-----------------------------------------------
# FILTRADO por estados
covid.mx.cdmx <- covid.mx.ss %>%
  filter(ENTIDAD_UM == 09)
# save(covid.mx.cdmx, file = "03_out/data/covid.mx.cdmx.RData")

#filtrado por estados 2020: cdmx.
covid.mx.cdmx.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 09)
  #save(covid.mx.cdmx.2020. file = "03_out/data/covid.mx.cdmx.2020.RData")
# procesamiento de datos de covid Mexio
##-----------------------------------------------
# se cargan las librerias
library(tidyr)
library(tidyverse)
library(dplyr)
##-----------------------------------------------
# se cargan los datos
covid.mx.ss <- read.csv("01_raw_data/COVID19MEXICO.csv") # covid mexico secretaria de salud
str(covid.mx.ss)
##-----------------------------------------------
# FILTRADO por estados
covid.mx.cdmx <- covid.mx.ss %>%
  filter(ENTIDAD_UM == 09)
# save(covid.mx.cdmx, file = "03_out/data/covid.mx.cdmx.RData")

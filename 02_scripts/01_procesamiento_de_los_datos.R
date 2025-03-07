# Procesamiento de los datos
##-----------------------------------------------
# librerias necesarias
library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
##-----------------------------------------------
# se cargan los datos de covid de todo el mundo
# covid <- owid_covid()
str(covid)
# se filtran para los datos de Mexico. Depende del pais, el iso_code
covid.mx <- covid %>% filter(iso_code=="MEX")
# se guardan los datos de covid Mexico
# save(covid.mx, file = "03_out/data/covid.mx.owid.RData")
# para cargar estos datos:
load(file = "03_out/data/covid.mx.owid.RData") # nombre del objeto: covid.mx


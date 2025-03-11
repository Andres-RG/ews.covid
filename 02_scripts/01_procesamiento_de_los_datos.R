# Procesamiento de los datos
##-----------------------------------------------
# librerias necesarias
library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
##-----------------------------------------------
# se cargan los datos de covid de todo el mundo. Los datos se descargaron para 
# no entrar al servidor
covid <- read_csv("01_raw_data/owid-covid-data.csv")
str(covid)
# se filtran para los datos de Mexico. Depende del pais, el iso_code
covid.mx <- covid %>% filter(iso_code=="MEX")
# se guardan los datos de covid Mexico
# save(covid.mx, file = "03_out/data/covid.mx.owid.RData")
# para cargar estos datos:
load(file = "03_out/data/covid.mx.owid.RData") # nombre del objeto: covid.mx
##-----------------------------------------------

# NOTA: Para filtrar para disntintos paises, cambiar el iso_code
# eje:
#       
# "REU"     
# "TKM"     
# "OWID_WLS"
# "GNB"     
# "PRI"     
# "OWID_NIR"
# "ARG"     
# "UKR"     
# "TWN"     
# "VIR"

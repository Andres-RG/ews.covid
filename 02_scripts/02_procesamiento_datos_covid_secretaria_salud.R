# procesamiento de datos de covid Mexio
##-----------------------------------------------
# se cargan las librerias
library(tidyr)
library(tidyverse)
library(dplyr)
##-----------------------------------------------
# se cargan los datos
covid.mx.ss <- read_csv("01_raw_data/COVID19MEXICO.csv") # covid mexico secretaria de salud
str(covid.mx.ss)
##-----------------------------------------------
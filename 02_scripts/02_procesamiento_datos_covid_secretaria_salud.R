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
  #save(covid.mx.ss.2020, file = "03_out/data/covid.mx.ss.2020.RData")
  #load("03_out/data/covid.mx.ss.2020.RData")

#2021
#se carga de manera manual.
#descargar datos de: https://www.gob.mx/cms/uploads/attachment/file/753707/Cierre_Datos_abiertos_historicos_2021.pdf
  #COVID19MEXICO2021
  #str(COVID19MEXICO2021)
    #save(COVID19MEXICO2021, file = "03_out/data/COVID19MEXICO2021.RData")
covid.mx.ss.2021 <- COVID19MEXICO2021
  str(covid.mx.ss.2021)

#2022
covid.mx.ss.2022 <- read.csv("01_raw_data/COVID19MEXICO2022.csv")
  #save(covid.mx.ss.2022, file = "03_out/data/covid.mx.ss.2022.RData")

#2023
covid.mx.ss.2023 <- read.csv("01_raw_data/COVID19MEXICO23.csv")
  #save(covid.mx.ss.2023, file = "03_out/data/covid.mx.ss.2023.RData")


##-----------------------------------------------
# FILTRADO por estados: 2024-5
covid.mx.cdmx <- covid.mx.ss %>%
  filter(ENTIDAD_UM == 09)
# save(covid.mx.cdmx, file = "03_out/data/covid.mx.cdmx.RData")

#filtrado por estados 2020: cdmx.
covid.mx.cdmx.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 09)
  #save(covid.mx.cdmx.2020, file = "03_out/data/covid.mx.cdmx.2020.RData")

#filtrado 2021.
covid.mx.cdmx.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 09)
  #save(covid.mx.cdmx.2021, file = "03_out/data/covid.mx.cdmx.2021.RData")
  
##filtrado por estados 2023: cdmx.
covid.mx.cdmx.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 09)
  #save(covid.mx.cdmx.2023, file = "03_out/data/covid.mx.cdmx.2023.RData")
  
#filtrado por estados 2022: cdmx.
covid.mx.cdmx.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 09)
  #save(covid.mx.cdmx.2022, file = "03_out/data/covid.mx.cdmx.2022.RData")  
  
##-----------------------------------------------
#FILTRADO POR ESTADOS.
#2024-5
#vz---veracruz.
#mantienen la clasificacion final flu!!!!
covid.mx.vz.2024 <- covid.mx.ss %>% #2024-5
  filter(ENTIDAD_UM == 30)
  #save(covid.mx.vz.2024, file = "03_out/data/covid.mx.veracruz.2024-5.RData")

#2023
  #load("03_out/data/covid.mx.ss.2023.RData")
covid.mx.vz.2023 <- covid.mx.ss.2023 %>%
  filter(ENTIDAD_UM == 30)
  #save(covid.mx.vz.2023, file = "03_out/data/covid.mx.vz.2023.RData")
  
#2022
  #load("03_out/data/covid.mx.ss.2022.RData")
covid.mx.vz.2022 <- covid.mx.ss.2022 %>%
  filter(ENTIDAD_UM == 30)  
  #save(covid.mx.vz.2022, file = "03_out/data/covid.mx.vz.2022.RData")
  
#filtrado 2021.
covid.mx.vz.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 30)
  #save(covid.mx.vz.2021, file = "03_out/data/covid.mx.vz.2021.RData")

#2020
  #load("03_out/data/covid.mx.ss.2020.RData")
covid.mx.vz.2020 <- covid.mx.ss.2020 %>%
  filter(ENTIDAD_UM == 30)  
  #save(covid.mx.vz.2020, file = "03_out/data/covid.mx.vz.2020.RData")

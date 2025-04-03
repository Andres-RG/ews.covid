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
  #save(covid.mx.ss, file = "03_out/data/covid.mx.sS.RData")

load("03_out/")

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
save(covid.mx.ss.2021, file = "03_out/data/covid.mx.ss.2021.RData")

  str(covid.mx.ss.2021)

#2022
covid.mx.ss.2022 <- read.csv("01_raw_data/COVID19MEXICO2022.csv")
  #save(covid.mx.ss.2022, file = "03_out/data/covid.mx.ss.2022.RData")

#2023
covid.mx.ss.2023 <- read.csv("01_raw_data/COVID19MEXICO23.csv")
  #save(covid.mx.ss.2023, file = "03_out/data/covid.mx.ss.2023.RData")


##-----------------------------------------------
# FILTRADO por estados CDMX
#2024-5
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

################################################################################
#JALISCO.
#2024-5
#jc---jalisco 14
#mantienen la clasificacion final flu!!!!
covid.mx.jc.2024 <- covid.mx.ss %>% #2024-5
  filter(ENTIDAD_UM == 14)
  #save(covid.mx.jc.2024, file = "03_out/data/covid.mx.jc.2024.RData")

#2023
covid.mx.jc.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 14)
  #save(covid.mx.jc.2023, file ="03_out/data/covid.mx.jc.2023.RData")
  
#2022
covid.mx.jc.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 14)
  #save(covid.mx.jc.2022, file ="03_out/data/covid.mx.jc.2022.RData")
  
#2021
covid.mx.jc.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 14)
  #save(covid.mx.jc.2021, file ="03_out/data/covid.mx.jc.2021.RData")

#2020
covid.mx.jc.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 14)
  #save(covid.mx.jc.2020, file ="03_out/data/covid.mx.jc.2020.RData")

################################################################################
#QUERETARO
#2024-5
#qt--queretaro-22
covid.mx.qt.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 22)
  #save(covid.mx.qt.2024, file = "03_out/data/covid.mx.qt.2024.RData")  

#2023
covid.mx.qt.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 22)
  #save(covid.mx.qt.2023, file ="03_out/data/covid.mx.qt.2023.RData")

#2022
covid.mx.qt.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 22)
  #save(covid.mx.qt.2022, file = "03_out/data/covid.mx.qt.2022.RData")
  
#2021
covid.mx.qt.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 22)
  #save(covid.mx.qt.2021, file = "03_out/data/covid.mx.qt.2021.RData")

#2020
covid.mx.qt.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 22)
  #save(covid.mx.qt.2020, file = "03_out/data/covid.mx.qt.2020.RData")

#################################################################################
#AGUASCALIENTES.
#as---01

#2024
covid.mx.as.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 01)
  #save(covid.mx.as.2024, file = "03_out/data/covid.mx.as.2024.RData")  

#2023
covid.mx.as.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 01)
  #save(covid.mx.as.2023, file ="03_out/data/covid.mx.as.2023.RData")

#2022
covid.mx.as.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 01)
  #save(covid.mx.as.2022, file = "03_out/data/covid.mx.as.2022.RData")

#2021
covid.mx.as.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 01)
  #save(covid.mx.as.2021, file = "03_out/data/covid.mx.as.2021.RData")

#2020
covid.mx.as.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 01)
  #save(covid.mx.as.2020, file = "03_out/data/covid.mx.as.2020.RData")

################################################################################
#BAJA CALIFORNIA.
#BC---02
  
#2024
covid.mx.bc.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 02)
  #save(covid.mx.bc.2024, file = "03_out/data/covid.mx.bc.2024.RData")  
  
#2023
covid.mx.bc.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 02)
  #save(covid.mx.bc.2023, file ="03_out/data/covid.mx.bc.2023.RData")
  
#2022
covid.mx.bc.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 02)
  #save(covid.mx.bc.2022, file = "03_out/data/covid.mx.bc.2022.RData")
  
#2021
covid.mx.bc.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 02)
  #save(covid.mx.as.2021, file = "03_out/data/covid.mx.bc.2021.RData")
  
#2020
covid.mx.bc.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 02)
  #save(covid.mx.bc.2020, file = "03_out/data/covid.mx.bc.2020.RData")

################################################################################
#BAJA CALIFORNIA SUR.
#BS--03
#2024
covid.mx.bs.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 03)
  #save(covid.mx.bs.2024, file = "03_out/data/covid.mx.bs.2024.RData")  

#2023
covid.mx.bs.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 03)
  #save(covid.mx.bs.2023, file ="03_out/data/covid.mx.bs.2023.RData")

#2022
covid.mx.bs.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 03)
  #save(covid.mx.bs.2022, file = "03_out/data/covid.mx.bs.2022.RData")

#2021: 
covid.mx.bs.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 03)
  #save(covid.mx.bs.2021, file = "03_out/data/covid.mx.bs.2021.RData")

#2020
covid.mx.bs.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 03)
  #save(covid.mx.bs.2020, file = "03_out/data/covid.mx.bs.2020.RData")

################################################################################
#CAMPECHE
#CC----04
#2024
covid.mx.cc.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 04)
  #save(covid.mx.cc.2024, file = "03_out/data/covid.mx.cc.2024.RData")  

#2023
covid.mx.cc.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 04)
  #save(covid.mx.cc.2023, file ="03_out/data/covid.mx.cc.2023.RData")

#2022
covid.mx.cc.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 04)
  #save(covid.mx.cc.2022, file = "03_out/data/covid.mx.cc.2022.RData")

#2021
covid.mx.cc.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 04)
  #save(covid.mx.cc.2021, file = "03_out/data/covid.mx.cc.2021.RData")

#2020
covid.mx.cc.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 04)
  #save(covid.mx.cc.2020, file = "03_out/data/covid.mx.cc.2020.RData")
##################################################################################
#COAHUILA DE ZARAGOZA
#CL---05
#2024
covid.mx.cl.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 05)
  #save(covid.mx.cl.2024, file = "03_out/data/covid.mx.cl.2024.RData")  

#2023
covid.mx.cl.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 05)
  #save(covid.mx.cl.2023, file ="03_out/data/covid.mx.cl.2023.RData")

#2022
covid.mx.cl.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 05)
  #save(covid.mx.cl.2022, file = "03_out/data/covid.mx.cl.2022.RData")

#2021
covid.mx.cl.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 05)
  #save(covid.mx.cl.2021, file = "03_out/data/covid.mx.cl.2021.RData")

#2020
covid.mx.cl.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 05)
  #save(covid.mx.cl.2020, file = "03_out/data/covid.mx.cl.2020.RData")
################################################################################

################################################################################
#Guanajuato
#11	GUANAJUATO	GT
#2024
covid.mx.gt.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 11)
  #save(covid.mx.gt.2024, file = "03_out/data/covid.mx.gt.2024.RData")  

#2023
covid.mx.gt.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 11)
  #save(covid.mx.gt.2023, file ="03_out/data/covid.mx.gt.2023.RData")

#2022
covid.mx.gt.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 11)
  #save(covid.mx.gt.2022, file = "03_out/data/covid.mx.gt.2022.RData")

#2021 
covid.mx.gt.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 11)
  #save(covid.mx.gt.2021, file = "03_out/data/covid.mx.gt.2021.RData")

#2020
covid.mx.gt.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 11)
  #save(covid.mx.gt.2020, file = "03_out/data/covid.mx.gt.2020.RData")

################################################################################
#31	YUCATÁN	YN
#2024
covid.yucatan.2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 31)
  save(covid.yucatan.2024, file = "03_out/data/covid.yucatan.2024.RData")  

#2023
covid.yucatan.2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 31)
  save(covid.yucatan.2023, file ="03_out/data/covid.yucatan.2023.RData")

#2022
covid.yucatan.2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 31)
  save(covid.yucatan.2022, file = "03_out/data/covid.yucatan.2022.RData")

#2021 
covid.yucatan.2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 31)
  save(covid.yucatan.2021, file = "03_out/data/covid.yucatan.2021.RData")

#2020
covid.yucatan.2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 31)
  save(covid.yucatan.2020, file = "03_out/data/covid.yucatan.2020.RData")
################################################################################
#OAXACA
#20	OAXACA	OC
#2024
  covid_oaxaca_2024 <- covid.mx.ss %>% filter(ENTIDAD_UM == 20)
  save(covid_oaxaca_2024, file = "03_out/data/covid_oaxaca_2024.RData")  
  
  #2023
  covid_oaxaca_2023 <- covid.mx.ss.2023 %>% filter(ENTIDAD_UM == 20)
  save(covid_oaxaca_2023, file ="03_out/data/covid_oaxaca_2023.RData")
  
  #2022
  covid_oaxaca_2022 <- covid.mx.ss.2022 %>% filter(ENTIDAD_UM == 20)
  save(covid_oaxaca_2022, file = "03_out/data/covid_oaxaca_2022.RData")
  
  #2021 
  covid_oaxaca_2021 <- covid.mx.ss.2021 %>% filter(ENTIDAD_UM == 20 )
  save(covid_oaxaca_2021, file = "03_out/data/covid_oaxaca_2021.RData")
  
  #2020
  covid_oaxaca_2020 <- covid.mx.ss.2020 %>% filter(ENTIDAD_UM == 20)
  save(covid_oaxaca_2020, file = "03_out/data/covid_oaxaca_2020.RData")
  
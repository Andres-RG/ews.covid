###VERACRUZ###
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
  #load("03_out/data/covid.mx.vz.2020.RData")
  #covid.mx.vz.2020$CLASIFICACION_FINAL
casos_positivos_vz_2020 <- filter(covid.mx.vz.2020, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2020 =================================================

vz_2020 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_vz_2020$FECHA_SINTOMAS) ) {
  vz_2020 <- c(vz_2020, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_vz_2020 <- mutate(casos_positivos_vz_2020, positivos = vz_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_vz_2020,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_vz_2020 [,3] <- c(1:length(casos_positivos_vz_2020$FECHA_SINTOMAS))
colnames(casos_positivos_vz_2020)[3] <- "num.dia" 
casos_positivos_vz_2020

##------------------------------------------
#data frame: ews 2020
data_covid_ews_vz2020 <- data.frame(
  time = seq(1, length(casos_positivos_vz_2020$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_2020$positivos
)
##------------------------------------------
# ews univariados: cdmx 2020
  #ews_metrics <- c("SD","ar1","skew")

ews_vz_2020 <- uniEWS(data = data_covid_ews_vz2020,
                        metrics =  ews_metrics,
                        method = "expanding", 
                        burn_in = 10, 
                        threshold = 2,
                        tail.direction = "one.tailed")
plot(ews_vz_2020)
  # pdf("03_out/plots/ews_vz_2020.univariado.pdf", height = 8, width = 10)
  # plot(ews_vz_2020)
  # dev.off()

################################################################################
#2022
  #load("03_out/data/covid.mx.vz.2022.RData")
  #covid.mx.vz.2022$CLASIFICACION_FINAL
casos_positivos_vz_2022 <- filter(covid.mx.vz.2022, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#Casos por dia. Incidencia 2022 =================================================

vz_2022 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_vz_2022$FECHA_SINTOMAS) ) {
  vz_2022 <- c(vz_2022, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_vz_2022 <- mutate(casos_positivos_vz_2022, positivos = vz_2022) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_2022 <- aggregate(positivos~FECHA_SINTOMAS, 
                                     data = casos_positivos_vz_2022,
                                     FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_vz_2022 [,3] <- c(1:length(casos_positivos_vz_2022$FECHA_SINTOMAS))
colnames(casos_positivos_vz_2022)[3] <- "num.dia" 
casos_positivos_vz_2022

##------------------------------------------
#data frame: ews 2022
data_covid_ews_vz2022 <- data.frame(
  time = seq(1, length(casos_positivos_vz_2022$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_2022$positivos
)
##------------------------------------------
# ews univariados: cdmx 2022
#ews_metrics <- c("SD","ar1","skew")

ews_vz_2022 <- uniEWS(data = data_covid_ews_vz2022,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_vz_2022)
# pdf("03_out/plots/ews_vz_2022univariado.pdf", height = 8, width = 10)
# plot(ews_vz_2022)
# dev.off()



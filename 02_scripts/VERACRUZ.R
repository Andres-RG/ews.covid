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

casos_positivos_vz_re_2020 <- mutate(casos_positivos_vz_2020, positivos = vz_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_re_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_vz_re_2020,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_vz_re_2020 [,3] <- c(1:length(casos_positivos_vz_re_2020$FECHA_SINTOMAS))
colnames(casos_positivos_vz_re_2020)[3] <- "num.dia" 
casos_positivos_vz_re_2020

##------------------------------------------
#data frame: ews 2020
data_covid_ews_vz2020 <- data.frame(
  time = seq(1, length(casos_positivos_vz_re_2020$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_re_2020$positivos
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
  #pdf("03_out/plots/ews_vz_2020.univariado.pdf", height = 8, width = 10)
  #plot(ews_vz_2020)
  #dev.off()

#################################################################################
#2021
load("03_out/data/covid.mx.vz.2021.RData")
  #covid.mx.vz.2021$CLASIFICACION_FINAL 
casos_positivos_vz_2021 <- filter(covid.mx.vz.2021, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2022 =================================================

vz_2021 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_vz_2021$FECHA_SINTOMAS) ) {
  vz_2021 <- c(vz_2021, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_vz_re_2021 <- mutate(casos_positivos_vz_2021, positivos = vz_2021) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_re_2021 <- aggregate(positivos~FECHA_SINTOMAS, 
                                     data = casos_positivos_vz_re_2021,
                                     FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_vz_re_2021 [,3] <- c(1:length(casos_positivos_vz_re_2021$FECHA_SINTOMAS))
colnames(casos_positivos_vz_re_2021)[3] <- "num.dia" 
casos_positivos_vz_re_2021

##------------------------------------------
#data frame: ews 2021
data_covid_ews_vz2021 <- data.frame(
  time = seq(1, length(casos_positivos_vz_re_2021$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_re_2021$positivos
)
##------------------------------------------
# ews univariados: cdmx 2022
#ews_metrics <- c("SD","ar1","skew")

ews_vz_2021 <- uniEWS(data = data_covid_ews_vz2021,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_vz_2021)
  #pdf("03_out/plots/ews_vz_2021.univariado.pdf", height = 8, width = 10)
  #plot(ews_vz_2021)
  #dev.off()

################################################################################
#2023
  load("03_out/data/covid.mx.vz.2023.RData")
  #covid.mx.vz.2023$CLASIFICACION_FINAL
casos_positivos_vz_2023 <- filter(covid.mx.vz.2023, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#Casos por dia. Incidencia 2023 =================================================

vz_2023 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_vz_2023$FECHA_SINTOMAS) ) {
  vz_2023 <- c(vz_2023, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_vz_re_2023 <- mutate(casos_positivos_vz_2023, positivos = vz_2023) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_re_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                     data = casos_positivos_vz_re_2023,
                                     FUN = sum)

# agrega la columna con el numero del dia.
casos_positivos_vz_re_2023[,3] <- c(1:length(casos_positivos_vz_re_2023$FECHA_SINTOMAS))
colnames(casos_positivos_vz_re_2023)[3] <- "num.dia" 
casos_positivos_vz_re_2023

##------------------------------------------
#data frame: ews 2023
data_covid_ews_vz2023 <- data.frame(
  time = seq(1, length(casos_positivos_vz_re_2023$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_re_2023$positivos
)
##------------------------------------------
# ews univariados: cdmx 2023
#ews_metrics <- c("SD","ar1","skew")

ews_vz_uni_2023 <- uniEWS(data = data_covid_ews_vz2023,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_vz_uni_2023)
#esta es la grafica que no marca ninguna señal.
data.uni.ews(ews_vz_uni_2023) ->ews_data_veracruz_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_veracruz_2023)-> ews_data_veracruz_plot_2023

#OBJETO GGPLOT: 
ews_data_veracruz_plot_2023 +labs(title = "VERACRUZ 2023", 
                                  subtitle = "NO marca ninguna señal")

 #pdf("03_out/plots/ews_vz_uni_2023.univariado.pdf", height = 8, width = 10)
 #plot(ews_vz_uni_2023)
 #dev.off()

################################################################################
#2024-5
  #load("03_out/data/covid.mx.veracruz.2024-5.RData")
#nombre de la base de datos:  
  #covid.mx.vz.2024$CLASIFICACION_FINAL_COVID #mantienen los mismos numeros.
casos_positivos_vz_2024 <- filter(covid.mx.vz.2024, CLASIFICACION_FINAL_COVID == 1 |
                                    CLASIFICACION_FINAL_COVID == 2 |
                                    CLASIFICACION_FINAL_COVID == 3 )
#Casos por dia. Incidencia 2024 =================================================

vz_2024 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_vz_2024$FECHA_SINTOMAS) ) {
  vz_2024 <- c(vz_2024, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_vz_re_2024 <- mutate(casos_positivos_vz_2024, positivos = vz_2024) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_vz_re_2024 <- aggregate(positivos~FECHA_SINTOMAS, 
                                     data = casos_positivos_vz_re_2024,
                                     FUN = sum)

# agrega la columna con el numero del dia.
casos_positivos_vz_re_2024[,3] <- c(1:length(casos_positivos_vz_re_2024$FECHA_SINTOMAS))
colnames(casos_positivos_vz_re_2024)[3] <- "num.dia" 
casos_positivos_vz_re_2024

##------------------------------------------
#data frame: ews 2024
data_covid_ews_vz2024 <- data.frame(
  time = seq(1, length(casos_positivos_vz_re_2024$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_vz_re_2024$positivos
)
##------------------------------------------
# ews univariados: cdmx 2024
#ews_metrics <- c("SD","ar1","skew")

ews_vz_2024 <- uniEWS(data = data_covid_ews_vz2024,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_vz_2024)
  #pdf("03_out/plots/ews_vz_2024.univariado.pdf", height = 8, width = 10)
  #plot(ews_vz_2024)
  #dev.off()


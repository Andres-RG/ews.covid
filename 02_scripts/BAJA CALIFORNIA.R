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

#Casos por dia. Incidencia 2020 
bc_2020 <- c()
for (i in 1:length(casos_positivos_bc_2020$FECHA_SINTOMAS)) {
  bc_2020 <-c(bc_2020, 1)
}
positivos_bc_re_2020 <- mutate(casos_positivos_bc_2020, positivos = bc_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_bc_re_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_bc_re_2020,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_bc_re_2020 [,3] <- c(1:length(positivos_bc_re_2020$FECHA_SINTOMAS))
colnames(positivos_bc_re_2020)[3] <- "num.dia" 
positivos_bc_re_2020

#data frame univariadas.
baja_datosews_2020 <- data.frame(
  time = seq(1, length(positivos_bc_re_2020$FECHA_SINTOMAS), 1) ,
  casos = positivos_bc_re_2020$positivos)

#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

baja_ews_2020 <- uniEWS(data = baja_datosews_2020,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed"
)
plot(baja_ews_2020)

data.uni.ews(baja_ews_2020) ->ews_data_bajacalifornia_2020 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2020)-> ews_data_bajacalifornia_plot_2020

#OBJETO GGPLOT: 
ews_data_bajacalifornia_plot_2020 +labs(title = "BC 2020")

################################################################################
#2021
load("03_out/data/covid.mx.bc.2021.RData")
  #covid.mx.bc.2021$CLASIFICACION_FINAL
casos_positivos_bc_2021 <- filter(covid.mx.bc.2021, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2020 
bc_2021 <- c()
for (i in 1:length(casos_positivos_bc_2021$FECHA_SINTOMAS)) {
  bc_2021 <-c(bc_2021, 1)
}
positivos_bc_re_2021 <- mutate(casos_positivos_bc_2021, positivos = bc_2021) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_bc_re_2021 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_bc_re_2021,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_bc_re_2021[,3] <- c(1:length(positivos_bc_re_2021$FECHA_SINTOMAS))
colnames(positivos_bc_re_2021)[3] <- "num.dia" 
positivos_bc_re_2021

#data frame univariadas.
baja_datosews_2021 <- data.frame(
  time = seq(1, length(positivos_bc_re_2021$FECHA_SINTOMAS), 1) ,
  casos = positivos_bc_re_2021$positivos)

#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

baja_ews_2021 <- uniEWS(data = baja_datosews_2021,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(baja_ews_2021)

data.uni.ews(baja_ews_2021) ->ews_data_bajacalifornia_2021 

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2021)-> ews_data_bajacalifornia_plot_2021

#OBJETO GGPLOT: 
ews_data_bajacalifornia_plot_2021 +labs(title = "BC 2021")

################################################################################
#2022
load("03_out/data/covid.mx.bc.2022.RData")
  #covid.mx.bc.2022$CLASIFICACION_FINAL
casos_positivos_bc_2022 <- filter(covid.mx.bc.2022, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2022
bc_2022 <- c()
for (i in 1:length(casos_positivos_bc_2022$FECHA_SINTOMAS)) {
  bc_2022 <-c(bc_2022, 1)
}
positivos_bc_re_2022 <- mutate(casos_positivos_bc_2022, positivos = bc_2022) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_bc_re_2022 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_bc_re_2022,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_bc_re_2022 [,3] <- c(1:length(positivos_bc_re_2022$FECHA_SINTOMAS))
colnames(positivos_bc_re_2022)[3] <- "num.dia" 
positivos_bc_re_2022

#data frame univariadas.
baja_datosews_2022 <- data.frame(
  time = seq(1, length(positivos_bc_re_2022$FECHA_SINTOMAS), 1) ,
  casos = positivos_bc_re_2022$positivos)

#vector con las metricas univariadas: ews_metrics---
    #ews_metrics <- c("SD","ar1","skew")

baja_ews_2022 <- uniEWS(data = baja_datosews_2022,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(baja_ews_2022)

data.uni.ews(baja_ews_2022) ->ews_data_bajacalifornia_2022

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2022)-> ews_data_bajacalifornia_plot_2022

#OBJETO GGPLOT: 
ews_data_bajacalifornia_plot_2022 +labs(title = "BC 2022")

################################################################################
#2023
load("03_out/data/covid.mx.bc.2023.RData")
  #covid.mx.bc.2023$CLASIFICACION_FINAL
casos_positivos_bc_2023 <- filter(covid.mx.bc.2023, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2023 
bc_2023 <- c()
for (i in 1:length(casos_positivos_bc_2023$FECHA_SINTOMAS)) {
  bc_2023 <-c(bc_2023, 1)
}
positivos_bc_re_2023 <- mutate(casos_positivos_bc_2023, positivos = bc_2023) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_bc_re_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_bc_re_2023,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_bc_re_2023[,3] <- c(1:length(positivos_bc_re_2023$FECHA_SINTOMAS))
colnames(positivos_bc_re_2023)[3] <- "num.dia" 
positivos_bc_re_2023

#data frame univariadas.
baja_datosews_2023 <- data.frame(
  time = seq(1, length(positivos_bc_re_2023$FECHA_SINTOMAS), 1) ,
  casos = positivos_bc_re_2023$positivos)


#vector con las metricas univariadas: ews_metrics---
  #ews_metrics <- c("SD","ar1","skew")

baja_ews_2023 <- uniEWS(data = baja_datosews_2023,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(baja_ews_2023)

data.uni.ews(baja_ews_2023) ->ews_data_bajacalifornia_2023 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2023)-> ews_data_bajacalifornia_plot_2023

#OBJETO GGPLOT: 
ews_data_bajacalifornia_plot_2023 +labs(title = "BC 2023")

################################################################################
#2024-5
load("03_out/data/covid.mx.bc.2024.RData")
  #covid.mx.bc.2024$CLASIFICACION_FINAL_COVID
casos_positivos_bc_2024 <- filter(covid.mx.bc.2024, CLASIFICACION_FINAL_COVID == 1 |
                                    CLASIFICACION_FINAL_COVID == 2 |
                                    CLASIFICACION_FINAL_COVID == 3 )

#Casos por dia. Incidencia 2020 
bc_2024 <- c()
for (i in 1:length(casos_positivos_bc_2024$FECHA_SINTOMAS)) {
  bc_2024 <-c(bc_2024, 1)
}
positivos_bc_re_2024 <- mutate(casos_positivos_bc_2024, positivos = bc_2024) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_bc_re_2024 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_bc_re_2024,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_bc_re_2024 [,3] <- c(1:length(positivos_bc_re_2024$FECHA_SINTOMAS))
colnames(positivos_bc_re_2024)[3] <- "num.dia" 
positivos_bc_re_2024

#data frame univariadas.
baja_datosews_2024 <- data.frame(
  time = seq(1, length(positivos_bc_re_2024$FECHA_SINTOMAS), 1) ,
  casos = positivos_bc_re_2024$positivos)

#vector con las metricas univariadas: ews_metrics---
  #ews_metrics <- c("SD","ar1","skew")

baja_ews_2024 <- uniEWS(data = baja_datosews_2024,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
plot(baja_ews_2024)

data.uni.ews(baja_ews_2024) ->ews_data_bajacalifornia_2024

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2024)-> ews_data_bajacalifornia_plot_2024

#OBJETO GGPLOT: 
ews_data_bajacalifornia_plot_2024 +labs(title = "BC 2024")

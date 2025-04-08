###QUERETARO###
#comparar el tiempo con el pico más cercano.
# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)

source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")

###############################################################################
#2020.
#BASES DE DATOS : qt
  load("03_out/data/covid.mx.qt.2020.RData")
  #covid.mx.qt.2020$CLASIFICACION_FINAL
casos_positivos_qt_2020 <- filter(covid.mx.qt.2020, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#INCIDENCIA DIARIA.
qt_2020 <- c()
for (i in 1:length(casos_positivos_qt_2020$FECHA_SINTOMAS)) {
  qt_2020 <-c(qt_2020, 1)
}
positivos_qt_re_2020 <- mutate(casos_positivos_qt_2020, positivos = qt_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_qt_re_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                        data = positivos_qt_re_2020,
                                        FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_qt_re_2020 [,3] <- c(1:length(positivos_qt_re_2020$FECHA_SINTOMAS))
colnames(positivos_qt_re_2020)[3] <- "num.dia" 
positivos_qt_re_2020

#data frame univariadas.
data_ews_qt_2020 <- data.frame(
  time = seq(1, length(positivos_qt_re_2020$FECHA_SINTOMAS), 1) ,
  casos = positivos_qt_re_2020$positivos
)
#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

ews_qt_2020 <- uniEWS(data = data_ews_qt_2020,
  metrics = ews_metrics,
  method = "expanding",
  burn_in = 10,
  threshold = 2,
  tail.direction = "one.tailed"
)
plot(ews_qt_2020)
  pdf("03_out/plots/ews_qt_2020.univariado.pdf", height = 8, width = 10)
  plot(ews_qt_2020)
  dev.off()

data.uni.ews(ews_qt_2020) ->ews_data_queretaro_2020 #extraer los datos/funcion.
ews_data_queretaro_2020$ar1
ews_data_queretaro_2020$threshold.crossed.SD

plot.univariate.ews.ggplot(ews_data_queretaro_2020)-> ews_data_queretaro_plot_2020

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_queretaro.20.1.pdf", height = 8, width = 10)
ews_data_queretaro_plot_2020 +labs(title = "QUERETARO 2020")+
  geom_hline(yintercept = c(2), col = "red")
dev.off()

#################################################################################
###2021:
load("03_out/data/covid.mx.qt.2021.RData")
#covid.mx.qt.2021$CLASIFICACION_FINAL
casos_positivos_qt_2021 <- filter(covid.mx.qt.2021, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#INCIDENCIA DIARIA.
qt_2021 <- c()
for (i in 1:length(casos_positivos_qt_2021$FECHA_SINTOMAS)) {
  qt_2021 <-c(qt_2021, 1)
}
positivos_qt_re_2021 <- mutate(casos_positivos_qt_2021, positivos = qt_2021) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_qt_re_2021 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_qt_re_2021,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_qt_re_2021 [,3] <- c(1:length(positivos_qt_re_2021$FECHA_SINTOMAS))
colnames(positivos_qt_re_2021)[3] <- "num.dia" 
positivos_qt_re_2021

#data frame univariadas.
data_ews_qt_2021 <- data.frame(
  time = seq(1, length(positivos_qt_re_2021$FECHA_SINTOMAS), 1) ,
  casos = positivos_qt_re_2021$positivos
)
#vector con las metricas univariadas: ews_metrics---ews_metrics <- c("SD","ar1","skew")
ews_qt_2021 <- uniEWS(data = data_ews_qt_2021,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_qt_2021)
ews_qt_2021$threshold
pdf("03_out/plots/ews_qt_2021.univariado.pdf", height = 8, width = 10)
  plot(ews_qt_2021)
  dev.off()
data.uni.ews(ews_qt_2021) ->ews_data_queretaro_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_queretaro_2021)-> ews_data_queretaro_plot_2021

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_queretaro.21.1.pdf", height = 8, width = 10)
ews_data_queretaro_plot_2021 +labs(title = "QUERETARO 2021")+
  geom_hline(yintercept = c(2), col ="red")
dev.off()  

########################################################################################
###2022
load("03_out/data/covid.mx.qt.2022.RData")
#covid.mx.qt.2022$CLASIFICACION_FINAL
casos_positivos_qt_2022 <- filter(covid.mx.qt.2022, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#INCIDENCIA DIARIA.
qt_2022 <- c()
for (i in 1:length(casos_positivos_qt_2022$FECHA_SINTOMAS)) {
  qt_2022 <-c(qt_2022, 1)
}
positivos_qt_re_2022 <- mutate(casos_positivos_qt_2022, positivos = qt_2022) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_qt_re_2022 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_qt_re_2022,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_qt_re_2022 [,3] <- c(1:length(positivos_qt_re_2022$FECHA_SINTOMAS))
colnames(positivos_qt_re_2022)[3] <- "num.dia" 
positivos_qt_re_2022

#data frame univariadas.
data_ews_qt_2022 <- data.frame(
  time = seq(1, length(positivos_qt_re_2022$FECHA_SINTOMAS), 1) ,
  casos = positivos_qt_re_2022$positivos
)
#vector con las metricas univariadas: ews_metrics---ews_metrics <- c("SD","ar1","skew")
ews_qt_2022 <- uniEWS(data = data_ews_qt_2022,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_qt_2022)
  pdf("03_out/plots/ews_qt_2022.univariado.pdf", height = 8, width = 10)
  plot(ews_qt_2022)
  dev.off()
data.uni.ews(ews_qt_2022) ->ews_data_queretaro_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_queretaro_2022)-> ews_data_queretaro_plot_2022

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_queretaro.22.1.pdf", height = 8, width = 10)
ews_data_queretaro_plot_2022 +labs(title = "QUERETARO 2022")+
  geom_hline(yintercept = c(2), col = "orange")
dev.off()
################################################################################
#2023.
load("03_out/data/covid.mx.qt.2023.RData")
  #covid.mx.qt.2023$CLASIFICACION_FINAL
casos_positivos_qt_2023 <- filter(covid.mx.qt.2023, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#INCIDENCIAS:
qt_2023 <- c()
for (i in 1:length(casos_positivos_qt_2023$FECHA_SINTOMAS)) {
  qt_2023 <-c(qt_2023, 1)
}
positivos_qt_re_2023 <- mutate(casos_positivos_qt_2023, positivos = qt_2023) 
# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_qt_re_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_qt_re_2023,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_qt_re_2023 [,3] <- c(1:length(positivos_qt_re_2023$FECHA_SINTOMAS))
colnames(positivos_qt_re_2023)[3] <- "num.dia" 
positivos_qt_re_2023

#DATA EWS UNIVARIADA:
data_ews_qt_2023 <- data.frame(
  time = seq(1, length(positivos_qt_re_2023$FECHA_SINTOMAS), 1) ,
  casos = positivos_qt_re_2023$positivos
)
#vector con las metricas univariadas: ews_metrics---ews_metrics <- c("SD","ar1","skew")
ews_qt_2023 <- uniEWS(data = data_ews_qt_2023,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_qt_2023)
  pdf("03_out/plots/ews_qt_2023.univariado.pdf", height = 8, width = 10)
  plot(ews_qt_2023)
  dev.off()
data.uni.ews(ews_qt_2023) ->ews_data_queretaro_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_queretaro_2023)-> ews_data_queretaro_plot_2023

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_queretaro.23.1.pdf", height = 8, width = 10)
ews_data_queretaro_plot_2023 +labs(title = "QUERETARO 2023")+
  geom_hline(yintercept = c(2))
dev.off()
####################################################################################
#2024-5.
load("03_out/data/covid.mx.qt.2024.RData")
  #covid.mx.qt.2024$CLASIFICACION_FINAL_COVID
casos_positivos_qt_2024 <- filter(covid.mx.qt.2024, CLASIFICACION_FINAL_COVID == 1 |
                                    CLASIFICACION_FINAL_COVID == 2 |
                                    CLASIFICACION_FINAL_COVID == 3 )
#INCIDENCIAS:
qt_2024 <- c()
for (i in 1:length(casos_positivos_qt_2024$FECHA_SINTOMAS)) {
  qt_2024 <-c(qt_2024, 1)
}
positivos_qt_re_2024 <- mutate(casos_positivos_qt_2024, positivos = qt_2024) 
# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_qt_re_2024 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_qt_re_2024,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_qt_re_2024 [,3] <- c(1:length(positivos_qt_re_2024$FECHA_SINTOMAS))
colnames(positivos_qt_re_2024)[3] <- "num.dia" 
positivos_qt_re_2024

#DATA EWS UNIVARIADA:
data_ews_qt_2024 <- data.frame(
  time = seq(1, length(positivos_qt_re_2024$FECHA_SINTOMAS), 1) ,
  casos = positivos_qt_re_2024$positivos
)
#vector con las metricas univariadas: ews_metrics---ews_metrics <- c("SD","ar1","skew")
ews_qt_2024 <- uniEWS(data = data_ews_qt_2024,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_qt_2024)
  pdf("03_out/plots/ews_qt_2024.univariado.pdf", height = 8, width = 10)
  plot(ews_qt_2024)
  dev.off()

data.uni.ews(ews_qt_2024) ->ews_data_queretaro_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_queretaro_2024)-> ews_data_queretaro_plot_2024

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_queretaro.23.1.pdf", height = 8, width = 10)
ews_data_queretaro_plot_2024 +labs(title = "QUERETARO 2023")+
  geom_hline(yintercept = c(2), col="red")
dev.off()

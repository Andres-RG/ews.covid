###GUANAJUATO###
#gt---11
# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)

source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")
################################################################################
#2020.
#BASES DE DATOS : qt
load("03_out/data/covid.mx.gt.2020.RData")
#covid.mx.qt.2020$CLASIFICACION_FINAL
positivos_guanajutao_20 <- filter(covid.mx.gt.2020, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )
#INCIDENCIA DIARIA.
gt_2020 <- c()
for (i in 1:length(positivos_guanajutao_20$FECHA_SINTOMAS)) {
  gt_2020 <-c(gt_2020, 1)
}
positivos_gt_re_20 <- mutate(positivos_guanajutao_20, positivos = gt_2020)

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_gt_re_20 <- aggregate(positivos~FECHA_SINTOMAS, 
                                  data = positivos_gt_re_20,
                                  FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_gt_re_20 [,3] <- c(1:length(positivos_gt_re_20$FECHA_SINTOMAS))
colnames(positivos_gt_re_20)[3] <- "num.dia" 
positivos_gt_re_20

#data frame univariadas.
data_gt_2020 <- data.frame(
  time = seq(1, length(positivos_gt_re_20$FECHA_SINTOMAS), 1) ,
  casos = positivos_gt_re_20$positivos
)
#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

ews_guanajuato_2020 <- uniEWS(data = data_gt_2020,
                      metrics = ews_metrics,
                      method = "expanding",
                      burn_in = 10,
                      threshold = 2,
                      tail.direction = "one.tailed"
)
plot(ews_guanajuato_2020)

ews_guanajuato_2020_1 <- uniEWS(data = data_gt_2020,
                              metrics = ews_metrics,
                              method = "expanding",
                              burn_in = 10,
                              threshold = 3,
                              tail.direction = "one.tailed"
)
plot(ews_guanajuato_2020_1)


data.uni.ews(ews_guanajuato_2020) ->ews_data_guanajuato_2020 #extraer los datos/funcion.
ews_data_guanajuato_2020$time
plot.univariate.ews.ggplot(ews_data_guanajuato_2020)-> ews_data_guanauato_plot_2020

#OBJETO GGPLOT: 
ews_data_guanauato_plot_2020 +labs(title = "GUANAJUATO 2020")+
  geom_hline(yintercept = c(2), col="red")

################################################################################
#2022:
load("03_out/data/covid.mx.gt.2022.RData")
  #covid.mx.gt.2022$CLASIFICACION_FINAL
positivos_guanajuato_22 <- filter(covid.mx.gt.2022, CLASIFICACION_FINAL == 1|
                                  CLASIFICACION_FINAL == 2|
                                  CLASIFICACION_FINAL == 3)

gt_2022 <- c()
for ( i in 1:length(positivos_guanajuato_22$FECHA_SINTOMAS)) {
  gt_2022 <-c(gt_2022, 1)
}

#INCIDENCIA DIARIA.
positivos_gt_re_22 <- mutate(positivos_guanajuato_22, positivos = gt_2022)

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_gt_re_22 <- aggregate(positivos~FECHA_SINTOMAS, 
                                data = positivos_gt_re_22,
                                FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_gt_re_22[,3] <- c(1:length(positivos_gt_re_22$FECHA_SINTOMAS))
colnames(positivos_gt_re_22)[3] <- "num.dia" 
positivos_gt_re_22

#data frame univariadas.
data_gt_2022 <- data.frame(
  time = seq(1, length(positivos_gt_re_22$FECHA_SINTOMAS), 1) ,
  casos = positivos_gt_re_22$positivos
)

#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

ews_guanajuato_2022 <- uniEWS(data = data_gt_2022,
                              metrics = ews_metrics,
                              method = "expanding",
                              burn_in = 10,
                              threshold = 2,
                              tail.direction = "one.tailed"
)
plot(ews_guanajuato_2022)

data.uni.ews(ews_guanajuato_2022) ->ews_data_guanajuato_2022 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_guanajuato_2022)-> ews_data_guanauato_plot_2022

#OBJETO GGPLOT: 
ews_data_guanauato_plot_2022 +labs(title = "GUANAJUATO 2022")+
  geom_hline(yintercept = c(2), col="red")

################################################################################
#2023
load("03_out/data/covid.mx.gt.2023.RData")
#covid.mx.gt.2022$CLASIFICACION_FINAL
positivos_guanajuato_23 <- filter(covid.mx.gt.2023, CLASIFICACION_FINAL == 1|
                                    CLASIFICACION_FINAL == 2|
                                    CLASIFICACION_FINAL == 3)

gt_2023 <- c()
for ( i in 1:length(positivos_guanajuato_23$FECHA_SINTOMAS)) {
  gt_2023 <-c(gt_2023, 1)
}

#INCIDENCIA DIARIA.
positivos_gt_re_23 <- mutate(positivos_guanajuato_23, positivos = gt_2023)

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_gt_re_23 <- aggregate(positivos~FECHA_SINTOMAS, 
                                data = positivos_gt_re_23,
                                FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_gt_re_23[,3] <- c(1:length(positivos_gt_re_23$FECHA_SINTOMAS))
colnames(positivos_gt_re_23)[3] <- "num.dia" 
positivos_gt_re_23

#data frame univariadas.
data_gt_2023 <- data.frame(
  time = seq(1, length(positivos_gt_re_23$FECHA_SINTOMAS), 1) ,
  casos = positivos_gt_re_23$positivos
)

#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

ews_guanajuato_2023 <- uniEWS(data = data_gt_2023,
                              metrics = ews_metrics,
                              method = "expanding",
                              burn_in = 10,
                              threshold = 2,
                              tail.direction = "one.tailed"
)
plot(ews_guanajuato_2023)

data.uni.ews(ews_guanajuato_2023) ->ews_data_guanajuato_2023 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_guanajuato_2023)-> ews_data_guanauato_plot_2023

#OBJETO GGPLOT: 
ews_data_guanauato_plot_2023 +labs(title = "GUANAJUATO 2023")+
  geom_hline(yintercept = c(2), col="red")
################################################################################
#2024-5
load("03_out/data/covid.mx.gt.2024.RData")
  covid.mx.gt.2024$CLASIFICACION_FINAL_COVID
positivos_guanajuato_24 <- filter(covid.mx.gt.2024, CLASIFICACION_FINAL_COVID == 1|
                                    CLASIFICACION_FINAL_COVID == 2|
                                    CLASIFICACION_FINAL_COVID == 3)

gt_2024 <- c()
for ( i in 1:length(positivos_guanajuato_24$FECHA_SINTOMAS)) {
  gt_2024 <-c(gt_2024, 1)
}

#INCIDENCIA DIARIA.
positivos_gt_re_24 <- mutate(positivos_guanajuato_24, positivos = gt_2024)

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
positivos_gt_re_24 <- aggregate(positivos~FECHA_SINTOMAS, 
                                data = positivos_gt_re_24,
                                FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
positivos_gt_re_24[,3] <- c(1:length(positivos_gt_re_24$FECHA_SINTOMAS))
colnames(positivos_gt_re_24)[3] <- "num.dia" 
positivos_gt_re_24

#data frame univariadas.
data_gt_2024 <- data.frame(
  time = seq(1, length(positivos_gt_re_24$FECHA_SINTOMAS), 1) ,
  casos = positivos_gt_re_24$positivos
)

#vector con las metricas univariadas: ews_metrics---
ews_metrics <- c("SD","ar1","skew")

ews_guanajuato_2024 <- uniEWS(data = data_gt_2024,
                              metrics = ews_metrics,
                              method = "expanding",
                              burn_in = 10,
                              threshold = 2,
                              tail.direction = "one.tailed"
)
plot(ews_guanajuato_2024)

data.uni.ews(ews_guanajuato_2024) ->ews_data_guanajuato_2024 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_guanajuato_2024)-> ews_data_guanauato_plot_2024

#OBJETO GGPLOT: 
ews_data_guanauato_plot_2024 +labs(title = "GUANAJUATO 2024-5")+
  geom_hline(yintercept = c(2), col="red")

###AGUASCALIENTES###
#CODIGO: as

#2020
load("03_out/data/covid.mx.as.2020.RData")
  #covid.mx.as.2020$CLASIFICACION_FINAL
positivo_aguascalientes_2020 <- filter(covid.mx.as.2020, CLASIFICACION_FINAL == 1 |
                                   CLASIFICACION_FINAL == 2 |
                                   CLASIFICACION_FINAL == 3 )

as_2020 <-c()
for (i in 1:length(positivo_aguascalientes_2020$FECHA_SINTOMAS) ) {
  as_2020 <- c(as_2020, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

aguascalientes_positivos_conteo_2020 <- mutate(positivo_aguascalientes_2020, 
                                               positivos = as_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
aguascalientes_positivos_conteo_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                           data = aguascalientes_positivos_conteo_2020,
                                           FUN = sum)
#anotacion del numero de día.
aguascalientes_positivos_conteo_2020[,3] <- c(1:length(
  aguascalientes_positivos_conteo_2020$FECHA_SINTOMAS))
colnames(aguascalientes_positivos_conteo_2020)[3] <- "num.dia" 
aguascalientes_positivos_conteo_2020

#data frame ews: 2020 
covid_ews_as_2020 <- data.frame(
  time = seq(1, length(aguascalientes_positivos_conteo_2020$FECHA_SINTOMAS), 1) ,
  casos = aguascalientes_positivos_conteo_2020$positivos
)

#ews_metrics <- c("SD","ar1","skew")

ews_as_2020 <- uniEWS(data = covid_ews_as_2020,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_as_2020)


################################################################################
#2021
load("03_out/data/covid.mx.as.2021.RData")
  #covid.mx.as.2021$CLASIFICACION_FINAL
positivo_aguascalientes_2021 <- filter(covid.mx.as.2021, CLASIFICACION_FINAL == 1 |
                                         CLASIFICACION_FINAL == 2 |
                                         CLASIFICACION_FINAL == 3 )

as_2021 <-c()
for (i in 1:length(positivo_aguascalientes_2021$FECHA_SINTOMAS) ) {
  as_2021 <- c(as_2021, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

aguascalientes_positivos_conteo_2021 <- mutate(positivo_aguascalientes_2021, 
                                               positivos = as_2021) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
aguascalientes_positivos_conteo_2021 <- aggregate(positivos~FECHA_SINTOMAS, 
                                                  data = aguascalientes_positivos_conteo_2021,
                                                  FUN = sum)

aguascalientes_positivos_conteo_2021[,3] <- c(1:length(
  aguascalientes_positivos_conteo_2021$FECHA_SINTOMAS))

colnames(aguascalientes_positivos_conteo_2021)[3] <- "num.dia" 

aguascalientes_positivos_conteo_2021

#data frame ews: 2020 
covid_ews_as_2021 <- data.frame(
  time = seq(1, length(aguascalientes_positivos_conteo_2021$FECHA_SINTOMAS), 1) ,
  casos = aguascalientes_positivos_conteo_2021$positivos
)

#ews_metrics <- c("SD","ar1","skew")

ews_as_2021 <- uniEWS(data = covid_ews_as_2021,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_as_2021)

#intento graficas ggplot.
source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")
data.uni.ews(ews_as_2021) ->ews_data_jalisco_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_data_jalisco_2021)-> jalisco_ews_plot_2021

#OBJETO GGPLOT: 
jalisco_ews_plot_2021 


################################################################################
#2022
load("03_out/data/covid.mx.as.2022.RData")
  #covid.mx.as.2022$CLASIFICACION_FINAL
positivo_aguascalientes_2022 <- filter(covid.mx.as.2022, CLASIFICACION_FINAL == 1 |
                                         CLASIFICACION_FINAL == 2 |
                                         CLASIFICACION_FINAL == 3 )

as_2022 <-c()
for (i in 1:length(positivo_aguascalientes_2022$FECHA_SINTOMAS) ) {
  as_2022 <- c(as_2022, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

aguascalientes_positivos_conteo_2022 <- mutate(positivo_aguascalientes_2022, 
                                               positivos = as_2022) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
aguascalientes_positivos_conteo_2022 <- aggregate(positivos~FECHA_SINTOMAS, 
                                                  data = aguascalientes_positivos_conteo_2022,
                                                  FUN = sum)

aguascalientes_positivos_conteo_2022[,3] <- c(1:length(
  aguascalientes_positivos_conteo_2022$FECHA_SINTOMAS))

colnames(aguascalientes_positivos_conteo_2022)[3] <- "num.dia" 

aguascalientes_positivos_conteo_2022

#data frame ews: 2020 
covid_ews_as_2022 <- data.frame(
  time = seq(1, length(aguascalientes_positivos_conteo_2022$FECHA_SINTOMAS), 1) ,
  casos = aguascalientes_positivos_conteo_2022$positivos
)

#ews_metrics <- c("SD","ar1","skew")

ews_as_2022 <- uniEWS(data = covid_ews_as_2022,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_as_2022)


################################################################################
#2023
load("03_out/data/covid.mx.as.2023.RData")
  #covid.mx.as.2023$CLASIFICACION_FINAL
positivo_aguascalientes_2023 <- filter(covid.mx.as.2023, CLASIFICACION_FINAL == 1 |
                                         CLASIFICACION_FINAL == 2 |
                                         CLASIFICACION_FINAL == 3 )
as_2023<-c()
for (i in 1:length(positivo_aguascalientes_2023$FECHA_SINTOMAS) ) {
  as_2023 <- c(as_2023, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

aguascalientes_positivos_conteo_2023 <- mutate(positivo_aguascalientes_2023, 
                                               positivos = as_2023) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
aguascalientes_positivos_conteo_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                                  data = aguascalientes_positivos_conteo_2023,
                                                  FUN = sum)

aguascalientes_positivos_conteo_2023[,3] <- c(1:length(
  aguascalientes_positivos_conteo_2023$FECHA_SINTOMAS))

colnames(aguascalientes_positivos_conteo_2023)[3] <- "num.dia" 

aguascalientes_positivos_conteo_2023

#data frame ews: 2020 
covid_ews_as_2023 <- data.frame(
  time = seq(1, length(aguascalientes_positivos_conteo_2023$FECHA_SINTOMAS), 1) ,
  casos = aguascalientes_positivos_conteo_2023$positivos
)

#ews_metrics <- c("SD","ar1","skew")

ews_as_2023 <- uniEWS(data = covid_ews_as_2023,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_as_2023)
#detectan cuando la curva baja.
################################################################################
#2024-5
load("03_out/data/covid.mx.as.2024.RData")
  #covid.mx.as.2024$CLASIFICACION_FINAL_COVID
positivo_aguascalientes_2024 <- filter(covid.mx.as.2024, CLASIFICACION_FINAL_COVID == 1 |
                                         CLASIFICACION_FINAL_COVID == 2 |
                                         CLASIFICACION_FINAL_COVID == 3 )
as_2024 <-c()
for (i in 1:length(positivo_aguascalientes_2024$FECHA_SINTOMAS) ) {
  as_2024 <- c(as_2024, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

aguascalientes_positivos_conteo_2024 <- mutate(positivo_aguascalientes_2024, 
                                               positivos = as_2024) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
aguascalientes_positivos_conteo_2024 <- aggregate(positivos~FECHA_SINTOMAS, 
                                                  data = aguascalientes_positivos_conteo_2024,
                                                  FUN = sum)

aguascalientes_positivos_conteo_2024[,3] <- c(1:length(
  aguascalientes_positivos_conteo_2024$FECHA_SINTOMAS))

colnames(aguascalientes_positivos_conteo_2024)[3] <- "num.dia" 

aguascalientes_positivos_conteo_2024

#data frame ews: 2020 
covid_ews_as_2024 <- data.frame(
  time = seq(1, length(aguascalientes_positivos_conteo_2024$FECHA_SINTOMAS), 1) ,
  casos = aguascalientes_positivos_conteo_2024$positivos
)

#ews_metrics <- c("SD","ar1","skew")

ews_as_2024 <- uniEWS(data = covid_ews_as_2024,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_as_2024)

source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")
###JALISCO###
#codigo: jc
#2020
load("03_out/data/covid.mx.jc.2020.RData")
  #covid.mx.jc.2020$CLASIFICACION_FINAL

jalisco_positivos_2020 <- filter(covid.mx.jc.2020, CLASIFICACION_FINAL == 1 |
                                   CLASIFICACION_FINAL == 2 |
                                   CLASIFICACION_FINAL == 3 )
jc_2020 <-c()
for (i in 1:length(jalisco_positivos_2020$FECHA_SINTOMAS) ) {
  jc_2020 <- c(jc_2020, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

jalisco_positivos_conteo_2020 <- mutate(jalisco_positivos_2020, positivos = jc_2020) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
jalisco_positivos_conteo_2020 <- aggregate(positivos~FECHA_SINTOMAS, 
                                        data = jalisco_positivos_conteo_2020,
                                        FUN = sum)

#anotacion del numero de día.
jalisco_positivos_conteo_2020[,3] <- c(1:length(jalisco_positivos_conteo_2020$FECHA_SINTOMAS))
colnames(jalisco_positivos_conteo_2020)[3] <- "num.dia" 
jalisco_positivos_conteo_2020

#data frame ews: 2020 JALISCO.
data_covid_ews_jc_2020 <- data.frame(
  time = seq(1, length(jalisco_positivos_conteo_2020$FECHA_SINTOMAS), 1) ,
  casos = jalisco_positivos_conteo_2020$positivos
)
##------------------------------------------
# ews univariados: 
  #ews_metrics <- c("SD","ar1","skew")

ews_jc_2020 <- uniEWS(data = data_covid_ews_jc_2020,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_jc_2020)
#  pdf("03_out/plots/ews_jc_2020.univariado.pdf", height = 8, width = 10)
#  plot(ews_jc_2020)
#  dev.off()

################################################################################
#2021
load("03_out/data/covid.mx.jc.2021.RData")
  #covid.mx.jc.2021$CLASIFICACION_FINAL
jalisco_positivos_2021 <- filter(covid.mx.jc.2021, CLASIFICACION_FINAL == 1|
                                 CLASIFICACION_FINAL == 2|
                                 CLASIFICACION_FINAL == 3)
jc_2021 <-c()
for (i in 1:length(jalisco_positivos_2021$FECHA_SINTOMAS) ) {
  jc_2021 <- c(jc_2021, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

jalisco_positivos_conteo_2021 <- mutate(jalisco_positivos_2021, positivos = jc_2021) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
jalisco_positivos_conteo_2021 <- aggregate(positivos~FECHA_SINTOMAS, 
                                           data = jalisco_positivos_conteo_2021,
                                           FUN = sum)
#anotacion del numero de día.
jalisco_positivos_conteo_2021[,3] <- c(1:length(jalisco_positivos_conteo_2021$FECHA_SINTOMAS))
colnames(jalisco_positivos_conteo_2021)[3] <- "num.dia" 
jalisco_positivos_conteo_2021

#data frame ews: 2020 JALISCO.
data_covid_ews_jc_2021 <- data.frame(
  time = seq(1, length(jalisco_positivos_conteo_2021$FECHA_SINTOMAS), 1) ,
  casos = jalisco_positivos_conteo_2021$positivos
)
##------------------------------------------
# ews univariados: 
#ews_metrics <- c("SD","ar1","skew")

ews_jc_2021 <- uniEWS(data = data_covid_ews_jc_2021,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_jc_2021)
  #pdf("03_out/plots/ews_jc_2021.univariado.pdf", height = 8, width = 10)
  #plot(ews_jc_2021)
  #dev.off()

################################################################################
#2022
load("03_out/data/covid.mx.jc.2022.RData")
  #covid.mx.jc.2022$CLASIFICACION_FINAL
jalisco_positivos_2022 <- filter(covid.mx.jc.2022, CLASIFICACION_FINAL == 1 |
                                   CLASIFICACION_FINAL == 2 |
                                   CLASIFICACION_FINAL == 3 )

jc_2022 <-c()
for (i in 1:length(jalisco_positivos_2022$FECHA_SINTOMAS) ) {
  jc_2022 <- c(jc_2022, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

jalisco_positivos_conteo_2022 <- mutate(jalisco_positivos_2022, positivos = jc_2022)

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
jalisco_positivos_conteo_2022 <- aggregate(positivos~FECHA_SINTOMAS, 
                                           data = jalisco_positivos_conteo_2022,
                                           FUN = sum)

#anotacion del numero de día.
jalisco_positivos_conteo_2022[,3] <- c(1:length(jalisco_positivos_conteo_2022$FECHA_SINTOMAS))
colnames(jalisco_positivos_conteo_2022)[3] <- "num.dia" 
jalisco_positivos_conteo_2022

#data frame ews: 2022 JALISCO.
data_covid_ews_jc_2022 <- data.frame(
  time = seq(1, length(jalisco_positivos_conteo_2022$FECHA_SINTOMAS), 1) ,
  casos = jalisco_positivos_conteo_2022$positivos
)
# ews univariados: 
#ews_metrics <- c("SD","ar1","skew")

ews_jc_2022 <- uniEWS(data = data_covid_ews_jc_2022,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_jc_2022)
  #pdf("03_out/plots/ews_jc_2022.univariado.pdf", height = 8, width = 10)
  #plot(ews_jc_2022)
  #dev.off()

################################################################################
#2023
load("03_out/data/covid.mx.jc.2023.RData")
  #covid.mx.jc.2023$CLASIFICACION_FINAL

jalisco_positivos_2023 <- filter(covid.mx.jc.2023, CLASIFICACION_FINAL == 1 |
                                   CLASIFICACION_FINAL == 2 |
                                   CLASIFICACION_FINAL == 3 )

jc_2023 <-c()
for (i in 1:length(jalisco_positivos_2023$FECHA_SINTOMAS) ) {
  jc_2023 <- c(jc_2023, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

jalisco_positivos_conteo_2023 <- mutate(jalisco_positivos_2023, positivos = jc_2023) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
jalisco_positivos_conteo_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                           data = jalisco_positivos_conteo_2023,
                                           FUN = sum)

#anotacion del numero de día.
jalisco_positivos_conteo_2023[,3] <- c(1:length(jalisco_positivos_conteo_2023$FECHA_SINTOMAS))
colnames(jalisco_positivos_conteo_2023)[3] <- "num.dia" 
jalisco_positivos_conteo_2023

#data frame ews: 2023 JALISCO.
data_covid_ews_jc_2023 <- data.frame(
  time = seq(1, length(jalisco_positivos_conteo_2023$FECHA_SINTOMAS), 1) ,
  casos = jalisco_positivos_conteo_2023$positivos
)
# ews univariados: 
#ews_metrics <- c("SD","ar1","skew")

ews_jc_2023 <- uniEWS(data = data_covid_ews_jc_2023,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_jc_2023)
  #pdf("03_out/plots/ews_jc_2023.univariado.pdf", height = 8, width = 10)
  #plot(ews_jc_2023)
  #dev.off()

################################################################################
#2024-5
load("03_out/data/covid.mx.jc.2024.RData")
  #covid.mx.jc.2024$CLASIFICACION_FINAL_COVID
jalisco_positivos_2024 <- filter(covid.mx.jc.2024, CLASIFICACION_FINAL_COVID == 1 |
                                   CLASIFICACION_FINAL_COVID == 2 |
                                   CLASIFICACION_FINAL_COVID == 3 )
jc_2024 <-c()
for (i in 1:length(jalisco_positivos_2024$FECHA_SINTOMAS) ) {
  jc_2024 <- c(jc_2024, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

jalisco_positivos_conteo_2024 <- mutate(jalisco_positivos_2024, positivos = jc_2024) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
jalisco_positivos_conteo_2024 <- aggregate(positivos~FECHA_SINTOMAS, 
                                           data = jalisco_positivos_conteo_2024,
                                           FUN = sum)
#anotacion del numero de día.
jalisco_positivos_conteo_2024[,3] <- c(1:length(jalisco_positivos_conteo_2024$FECHA_SINTOMAS))
colnames(jalisco_positivos_conteo_2024)[3] <- "num.dia" 
jalisco_positivos_conteo_2024

#data frame ews: 2023 JALISCO.
data_covid_ews_jc_2024 <- data.frame(
  time = seq(1, length(jalisco_positivos_conteo_2024$FECHA_SINTOMAS), 1) ,
  casos = jalisco_positivos_conteo_2024$positivos
)

# ews univariados: 
#ews_metrics <- c("SD","ar1","skew")

ews_jc_2024 <- uniEWS(data = data_covid_ews_jc_2024,
                      metrics =  ews_metrics,
                      method = "expanding", 
                      burn_in = 10, 
                      threshold = 2,
                      tail.direction = "one.tailed")
plot(ews_jc_2024)
#NO SEÑALES DE ALERTA.
  #pdf("03_out/plots/ews_jc_2024.univariado.pdf", height = 8, width = 10)
  #plot(ews_jc_2024)
  #dev.off()
data.uni.ews(ews_jc_2024) ->ews_jalisco24 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_jalisco24)-> jalisco_plot_24

#OBJETO GGPLOT: 
jalisco_plot_24 +labs(title = "jalisco 2024",
                      subtitle = "No detecta ews")

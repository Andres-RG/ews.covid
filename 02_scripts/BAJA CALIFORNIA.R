###BAJA CALIFORNIA###
# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)

source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")

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
pdf("03_out/plots/ews_u_baja.20.pdf", height = 8, width = 10)
plot(baja_ews_2020)
dev.off()

data.uni.ews(baja_ews_2020) ->ews_data_bajacalifornia_2020 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2020)-> ews_data_bajacalifornia_plot_2020

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.20.1.pdf", height = 8, width = 10)
plot(ews_data_bajacalifornia_plot_2020 +labs(title = "BC 2020")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

################################################################################
#2021
load("03_out/data/covid.mx.bc.2021.RData")
  #covid.mx.bc.2021$CLASIFICACION_FINAL
casos_positivos_bc_2021 <- filter(covid.mx.bc.2021, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

#Casos por dia. Incidencia 2021
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
pdf("03_out/plots/ews_u_baja.21.pdf", height = 8, width = 10)
plot(baja_ews_2021)
dev.off()

data.uni.ews(baja_ews_2021) ->ews_data_bajacalifornia_2021 

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2021)-> ews_data_bajacalifornia_plot_2021

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.21.1.pdf", height = 8, width = 10)
plot(ews_data_bajacalifornia_plot_2021 +labs(title = "BC 2021")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

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
pdf("03_out/plots/ews_u_baja.22.pdf", height = 8, width = 10)
plot(baja_ews_2022)
dev.off()

data.uni.ews(baja_ews_2022) ->ews_data_bajacalifornia_2022

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2022)-> ews_data_bajacalifornia_plot_2022

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.22.1.pdf", height = 8, width = 10)
plot(ews_data_bajacalifornia_plot_2022 +labs(title = "BC 2022")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

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
pdf("03_out/plots/ews_u_baja.23.pdf", height = 8, width = 10)
plot(baja_ews_2023)
dev.off()

data.uni.ews(baja_ews_2023) ->ews_data_bajacalifornia_2023 #extraer los datos/funcion.

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2023)-> ews_data_bajacalifornia_plot_2023

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.23.1.pdf", height = 8, width = 10)
plot(ews_data_bajacalifornia_plot_2023 +labs(title = "BC 2023")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

################################################################################
#2024-5
load("03_out/data/covid.mx.bc.2024.RData")
  #covid.mx.bc.2024$CLASIFICACION_FINAL_COVID
casos_positivos_bc_2024 <- filter(covid.mx.bc.2024, CLASIFICACION_FINAL_COVID == 1 |
                                    CLASIFICACION_FINAL_COVID == 2 |
                                    CLASIFICACION_FINAL_COVID == 3 )

#Casos por dia. Incidencia 2024-5
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
  casos = positivos_bc_re_2024$positivos)#vector con las metricas univariadas: ews_metrics---
  #ews_metrics <- c("SD","ar1","skew")

baja_ews_2024 <- uniEWS(data = baja_datosews_2024,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
pdf("03_out/plots/ews_u_baja.24.pdf", height = 8, width = 10)
plot(baja_ews_2024)
dev.off()

data.uni.ews(baja_ews_2024) ->ews_data_bajacalifornia_2024

plot.univariate.ews.ggplot(ews_data_bajacalifornia_2024)-> ews_data_bajacalifornia_plot_2024

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.24.1.pdf", height = 8, width = 10)
plot(ews_data_bajacalifornia_plot_2024 +labs(title = "BC 2024")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

################################################################################
###BAJA CALIFORNIA SUR###
##BS--03

#2020:
load("03_out/data/covid.mx.bs.2020.RData")

casos_positivos_bsur_2020 <- filter(covid.mx.bs.2020, CLASIFICACION_FINAL == 1 |
                                    CLASIFICACION_FINAL == 2 |
                                    CLASIFICACION_FINAL == 3 )

##------------------------------------------------------------------------------
avance <- function(datos){
  bc_20 <- c()
  for (i in 1:length(datos$FECHA_SINTOMAS)) {
    bc_20 <-c(bc_20, 1)
  }
  positivos_re_ <- mutate(datos, positivos = bc_20) 
  positivos_re_ <- aggregate(positivos~FECHA_SINTOMAS, 
                                    data = positivos_re_,
                                    FUN = sum)
  positivos_re_[,3] <- c(1:length(positivos_re_$FECHA_SINTOMAS))
  colnames(positivos_re_)[3] <- "num.dia" 
  
  return(positivos_re_)

}
##------------------------------------------------------------------------------

avance(casos_positivos_bsur_2020) ->bsur_incidencia_2020
bsur_incidencia_2020

bajasur_datosews_2020 <- data.frame(
  time = seq(1, length(bsur_incidencia_2020$FECHA_SINTOMAS), 1) ,
  casos = bsur_incidencia_2020$positivos)

#ews_metrics <- c("SD","ar1","skew")

bsur_ews_2020 <- uniEWS(data = bajasur_datosews_2020,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
#####################
pdf("03_out/plots/ews_u_baja.sur.20.pdf", height = 8, width = 10)
plot(bsur_ews_2020)
dev.off()

data.uni.ews(bsur_ews_2020) ->ews_bsur_2020 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_bsur_2020)-> ews_bsur_plot_2020

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.sur.20.1.pdf", height = 8, width = 10)
plot(ews_bsur_plot_2020 +labs(title = "BAJA CALIFORNIA", subtitle = "2020")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

#2021:
load("03_out/data/covid.mx.bs.2021.RData")

positivos_bsur_2021<-filter(covid.mx.bs.2021, CLASIFICACION_FINAL == 1 |
                              CLASIFICACION_FINAL == 2 |
                              CLASIFICACION_FINAL == 3 )

avance(positivos_bsur_2021) ->bsur_incidencia_2021
bsur_incidencia_2021

bajasur_datosews_2021 <- data.frame(
  time = seq(1, length(bsur_incidencia_2021$FECHA_SINTOMAS), 1) ,
  casos = bsur_incidencia_2021$positivos)

#ews_metrics <- c("SD","ar1","skew")

bsur_ews_2021 <- uniEWS(data = bajasur_datosews_2021,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
pdf("03_out/plots/ews_u_baja.sur.21.pdf", height = 8, width = 10)
plot(bsur_ews_2021)
dev.off()

data.uni.ews(bsur_ews_2021) ->ews_bsur_2021 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_bsur_2021)-> ews_bsur_plot_2021

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.sur.21.1.pdf", height = 8, width = 10)
plot(ews_bsur_plot_2021 +labs(title = "BAJA CALIFORNIA", subtitle = "2021")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

#2022
load("03_out/data/covid.mx.bs.2022.RData")

positivos_bsur_2022<-filter(covid.mx.bs.2022, CLASIFICACION_FINAL == 1 |
                              CLASIFICACION_FINAL == 2 |
                              CLASIFICACION_FINAL == 3 )

avance(positivos_bsur_2022) ->bsur_incidencia_2022
bsur_incidencia_2022

bajasur_datosews_2022 <- data.frame(
  time = seq(1, length(bsur_incidencia_2022$FECHA_SINTOMAS), 1) ,
  casos = bsur_incidencia_2022$positivos)

#ews_metrics <- c("SD","ar1","skew")

bsur_ews_2022 <- uniEWS(data = bajasur_datosews_2022,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
pdf("03_out/plots/ews_u_baja.sur.22.pdf", height = 8, width = 10)
plot(bsur_ews_2022)
dev.off()

data.uni.ews(bsur_ews_2022) ->ews_bsur_2022 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_bsur_2022)-> ews_bsur_plot_2022

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.sur.22.1.pdf", height = 8, width = 10)
plot(ews_bsur_plot_2022 +labs(title = "BAJA CALIFORNIA", subtitle = "2022")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

#2023
load("03_out/data/covid.mx.bs.2023.RData")
positivos_bsur_2023<-filter(covid.mx.bs.2023, CLASIFICACION_FINAL == 1 |
                              CLASIFICACION_FINAL == 2 |
                              CLASIFICACION_FINAL == 3 )

avance(positivos_bsur_2023) ->bsur_incidencia_2023
bsur_incidencia_2023

bajasur_datosews_2023 <- data.frame(
  time = seq(1, length(bsur_incidencia_2023$FECHA_SINTOMAS), 1) ,
  casos = bsur_incidencia_2023$positivos)

#ews_metrics <- c("SD","ar1","skew")

bsur_ews_2023 <- uniEWS(data = bajasur_datosews_2023,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
pdf("03_out/plots/ews_u_baja.sur.23.pdf", height = 8, width = 10)
plot(bsur_ews_2023)
dev.off()

data.uni.ews(bsur_ews_2023) ->ews_bsur_2023 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_bsur_2023)-> ews_bsur_plot_2023

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.sur.23.1.pdf", height = 8, width = 10)
plot(ews_bsur_plot_2023 +labs(title = "BAJA CALIFORNIA", subtitle = "2023")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

#2024-5
load("03_out/data/covid.mx.bs.2024.RData")
covid.mx.bs.2024$CLASIFICACION_FINAL_COVID

positivos_bsur_2024<-filter(covid.mx.bs.2024, CLASIFICACION_FINAL_COVID == 1 |
                              CLASIFICACION_FINAL_COVID == 2 |
                              CLASIFICACION_FINAL_COVID == 3 )

avance(positivos_bsur_2024) ->bsur_incidencia_2024
bsur_incidencia_2024

bajasur_datosews_2024 <- data.frame(
  time = seq(1, length(bsur_incidencia_2024$FECHA_SINTOMAS), 1) ,
  casos = bsur_incidencia_2024$positivos)

#ews_metrics <- c("SD","ar1","skew")

bsur_ews_2024 <- uniEWS(data = bajasur_datosews_2024,
                        metrics = ews_metrics,
                        method = "expanding",
                        burn_in = 10,
                        threshold = 2,
                        tail.direction = "one.tailed"
)
pdf("03_out/plots/ews_u_baja.sur.24.pdf", height = 8, width = 10)
plot(bsur_ews_2024)
dev.off()

data.uni.ews(bsur_ews_2024) ->ews_bsur_2024 #extraer los datos/funcion.
plot.univariate.ews.ggplot(ews_bsur_2024)-> ews_bsur_plot_2024

#OBJETO GGPLOT: 
pdf("03_out/plots/ews_u_baja.sur.24-5.1.pdf", height = 8, width = 10)
plot(ews_bsur_plot_2024 +labs(title = "BAJA CALIFORNIA", subtitle = "2024")+
  geom_hline(yintercept = c(2), col = "red"))
dev.off()

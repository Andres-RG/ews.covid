#CDMX:
load("03_out/data/covid.mx.cdmx.RData")
head(covid.mx.cdmx)
# covid.mx.cdmx$CLASIFICACION_FINAL_COVID: 3
covid.mx.cdmx.positivos <- covid.mx.cdmx[which(covid.mx.cdmx$CLASIFICACION_FINAL_COVID == "3"), ]
View(covid.mx.cdmx.positivos) #Casos positivos por muestra de PCR. 

# Librerias ===================================================================
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)
################################################################################
#2024-5
casos_positivos_re <- filter(covid.mx.cdmx, CLASIFICACION_FINAL_COVID  == 1 |
                               CLASIFICACION_FINAL_COVID  == 2 |
                               CLASIFICACION_FINAL_COVID  == 3 )

# 8. Casos por dia. Incidencia =================================================

pos <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re$FECHA_SINTOMAS) ) {
  pos <- c(pos, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_re_conteo <- mutate(casos_positivos_re, positivos = pos) 
# genera una nueva columna en la base de los casos_positivos_re
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos 
# los renglones. 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_re_conteo <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_re_conteo,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_re_conteo [,3] <- c(1:length(casos_positivos_re_conteo$FECHA_SINTOMAS))
colnames(casos_positivos_re_conteo)[3] <- "num.dia" 
casos_positivos_re_conteo

##------------------------------------------
#data frame: ews 2024-5
data_covid_ews_cdmx2025 <- data.frame(
  time = seq(1, length(casos_positivos_re_conteo$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_re_conteo$positivos
)
##------------------------------------------
# ews univariados: cdmx 2024-5
ews_metrics <- c("SD","ar1","skew")

ews_cdmx_2025 <- uniEWS(data = data_covid_ews_cdmx2025,
                              metrics =  ews_metrics,
                              method = "expanding", 
                              burn_in = 10, 
                              threshold = 2,
                              tail.direction = "one.tailed")
plot(ews_cdmx_2025)
# pdf("03_out/plots/ews_cdmx_2025.univariado.pdf", height = 8, width = 10)
# plot(ews_cdmx_2025)
# dev.off()



########################################################################################
#2020
#covid.mx.cdmx.2020$CLASIFICACION_FINAL: se mantiene 1, 2 y 3.

casos_positivos_re.2020 <- filter(covid.mx.cdmx.2020, CLASIFICACION_FINAL  == 1 |
                                    CLASIFICACION_FINAL  == 2 |
                                    CLASIFICACION_FINAL  == 3 )


##########################################################################################
#2023
#base de datos:
load("03_out/data/covid.mx.cdmx.2023.RData")
  #covid.mx.cdmx.2023$CLASIFICACION_FINAL: 
casos_positivos_re_2023 <- filter(covid.mx.cdmx.2023, CLASIFICACION_FINAL  == 1 |
                               CLASIFICACION_FINAL== 2 |
                               CLASIFICACION_FINAL== 3 )

# 8. Casos por dia. Incidencia =================================================

pos_2023 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re_2023$FECHA_SINTOMAS) ) {
  pos_2023 <- c(pos_2023, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_conteo_2023 <- mutate(casos_positivos_re_2023, positivos = pos_2023) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_conteo_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_conteo_2023,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_conteo_2023 [,3] <- c(1:length(casos_positivos_conteo_2023$FECHA_SINTOMAS))
colnames(casos_positivos_conteo_2023)[3] <- "num.dia" 
casos_positivos_conteo_2023

##------------------------------------------
#data frame: ews 2023
data_covid_ews_cdmx2023 <- data.frame(
  time = seq(1, length(casos_positivos_conteo_2023$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_conteo_2023$positivos
)
##------------------------------------------
# ews univariados: cdmx 2023
ews_metrics <- c("SD","ar1","skew")

ews_cdmx_2023 <- uniEWS(data = data_covid_ews_cdmx2023,
                        metrics =  ews_metrics,
                        method = "expanding", 
                        burn_in = 10, 
                        threshold = 2,
                        tail.direction = "one.tailed")
plot(ews_cdmx_2023)
  # pdf("03_out/plots/ews_cdmx_2023.univariado.pdf", height = 8, width = 10)
  # plot(ews_cdmx_2023)
  # dev.off()

##################################################################################
#2022
#base de datos:
load("03_out/data/covid.mx.cdmx.2022.RData")
  #covid.mx.cdmx.2022$CLASIFICACION_FINAL
casos_positivos_re_2022 <- filter(covid.mx.cdmx.2022, CLASIFICACION_FINAL  == 1 |
                                    CLASIFICACION_FINAL== 2 |
                                    CLASIFICACION_FINAL== 3 )

# 8. Casos por dia. Incidencia =================================================

pos_2022 <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re_2022$FECHA_SINTOMAS) ) {
  pos_2022 <- c(pos_2022, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector

casos_positivos_conteo_2022 <- mutate(casos_positivos_re_2022, positivos = pos_2022) 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_conteo_2023 <- aggregate(positivos~FECHA_SINTOMAS, 
                                         data = casos_positivos_conteo_2023,
                                         FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_conteo_2023 [,3] <- c(1:length(casos_positivos_conteo_2023$FECHA_SINTOMAS))
colnames(casos_positivos_conteo_2023)[3] <- "num.dia" 
casos_positivos_conteo_2023

##------------------------------------------
#data frame: ews 2023
data_covid_ews_cdmx2023 <- data.frame(
  time = seq(1, length(casos_positivos_conteo_2023$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_conteo_2023$positivos
)
##------------------------------------------
# ews univariados: cdmx 2023
ews_metrics <- c("SD","ar1","skew")

ews_cdmx_2023 <- uniEWS(data = data_covid_ews_cdmx2023,
                        metrics =  ews_metrics,
                        method = "expanding", 
                        burn_in = 10, 
                        threshold = 2,
                        tail.direction = "one.tailed")
plot(ews_cdmx_2023)
# pdf("03_out/plots/ews_cdmx_2023.univariado.pdf", height = 8, width = 10)
# plot(ews_cdmx_2023)
# dev.off()

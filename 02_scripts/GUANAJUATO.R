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

data.uni.ews(ews_guanajuato_2020) ->ews_data_guanajuato_2020 #extraer los datos/funcion.
ews_data_guanajuato_2020$time
plot.univariate.ews.ggplot(ews_data_guanajuato_2020)-> ews_data_guanauato_plot_2020

#OBJETO GGPLOT: 
ews_data_guanauato_plot_2020 +labs(title = "GUANAJUATO 2020")+
  geom_hline(yintercept = c(2), col="red")

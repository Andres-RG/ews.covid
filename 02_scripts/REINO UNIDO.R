#REINO UNIDO

library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(EWSmethods)
####USA####
#1.- FILTRADO DE DATOS:
#DATOS:
#covid <- owid_covid() # de aqui se carga la base de datos de owid

str(covid)
#FILTRADO:
covid_reino_unido <- covid %>%filter(iso_code== "GBR")
#PARA GUARDAR LA BASE DE DATOS POR PAIS:
#save(objeto, "nombre del archivo.RData")

#UNIVARIADAS:
df_covid_reino_unido <- data.frame(
  tiem = seq(1, length(covid_reino_unido$date), 1) ,
  covid_reino_unido[ ,5:16]
)

names(df_covid_reino_unido)

##VECTOR CON LAS METRICAS:
ews_metrics

################################################################################
names(df_covid_reino_unido[-c(1:10),c(1,3)]) 
#time
#new cases

ews_univariado_ru <- uniEWS(data = df_covid_reino_unido[-c(1:10),c(1,3)],
                               metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
                               method = "expanding", #VENTANA QUE SE EXPANDE
                               burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
                               threshold = 2, #VARIANZAS
                               tail.direction = "one.tailed") 
plot(ews_univariado_ru) 
# pdf("03_out/plots/ews_univariado_UK.nuevos.casos.pdf", height = 8, width = 10)
# plot(ews_univariado_ru)
# dev.off()

################################################################################
names(df_covid_reino_unido[-c(1:10),c(1,4)]) 
#time
#new cases smoothed

ews_univariado_ru_2<- uniEWS(data = df_covid_reino_unido[-c(1:10),c(1,4)] ,
                                 metrics =  ews_metrics,
                                 method = "expanding",
                                 burn_in = 10,
                                 threshold = 2,
                                 tail.direction = "one.tailed")

plot(ews_univariado_ru_2)
# pdf("03_out/plots/ews_univariado_UK.nuevos.smoothed.pdf", height = 8, width = 10)
# plot(ews_univariado_ru_2)
# dev.off()

##################################################################################
names(df_covid_reino_unido[-c(1:10),c(1,7)])
#tiempo
#new deaths smoothed

ews_univariado_ru_3<- uniEWS(data = df_covid_reino_unido[-c(1:10),c(1,7)] ,
                                metrics =  ews_metrics,
                                method = "expanding",
                                burn_in = 10,
                                threshold = 2,
                                tail.direction = "one.tailed")
plot(ews_univariado_ru_3)
# pdf("03_out/plots/ews_univariado_UK.nuevos.deaths.smoothed.pdf", height = 8, width = 10)
# plot(ews_univariado_ru_3)
# dev.off()

##################################################################################

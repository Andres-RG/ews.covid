######PAISES CON MÁS PRUEBAS PT1.#####
#DINAMARCA
#ITALIA
#INDIA


library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(EWSmethods)

####################################
####USA####
#1.- FILTRADO DE DATOS:
#DATOS:
  #covid <- owid_covid() # de aqui se carga la base de datos de owid
save(covid, file =  "covid_mundial.RData")
load("01_raw_data/covid_mundial.RData")


#cargar la nueva base de datos:
  #covid_datos <- read.csv("01_raw_data/owid-covid-data.csv")

str(covid)
head(covid)
#FILTRADO:
covid_usa <- covid %>% filter(iso_code== "USA") #volver a cargar los datos.
save(covid_usa, file =  "03_out/data/Covid USA.RData")

df_covid_usa <- data.frame(
  time = seq(1, length(covid_usa$date), 1)     ,
  covid_usa[ , 5:16]
)


head(df_covid_usa)
names(df_covid_usa)

##VECTOR CON LAS METRICAS:
ews_metrics <- c("SD","ar1","skew")
ews_metrics

##FUNCION PARA DATOS CON MUCHAS NA.
head(df_covid_usa[rowSums(is.na(df_covid_usa)) == 0,]) ## para eliminar reglones con NA
df_covid_usa<- df_covid_usa[rowSums(is.na(df_covid_usa)) == 0,]

################################################################################
names(df_covid_usa[-c(1:10),c(1,3)]) #para que solo este el tiempo
#y los nuevos casos. Lo necesario para realizar el analisis.
#tiempo
#nuevos casos.

##tiene datos faltantes: 
library(dplyr)
library(zoo)

 
# datos_usa_sin_na <- df_covid_usa[!is.na(df_covid_usa$new_cases), ] #NO NA

ews_univariado_usa <- uniEWS(data = df_covid_usa[-c(1:10),c(1,3)],
                             #tiempo y nuevos casos.
                               metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
                               method = "expanding", #VENTANA QUE SE EXPANDE
                               burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
                               threshold = 2, #VARIANZAS
                               tail.direction = "one.tailed") 
plot(ews_univariado_usa) 

pdf("03_out/plots/ews_univariado_usa.nuevos.casos.pdf", height = 8, width = 10)
plot(ews_univariado_usa)
dev.off()
####################################################################################

names(df_covid_usa[-c(1:10),c(1,4)])
#timpo
#nuevos casos suavizados.

ews_univariado_usa_2<- uniEWS(data = df_covid_usa[-c(1:10),c(1,4)] ,
                                metrics =  ews_metrics,
                                method = "expanding",
                                burn_in = 10,
                                threshold = 2,
                                tail.direction = "one.tailed")
plot(ews_univariado_usa_2)

pdf("03_out/plots/ews_univariado_usa.nuevos.casos.suavizado.pdf", height = 8, width = 10)
plot(ews_univariado_usa_2)
dev.off()

################################################################################
names(df_covid_usa[-c(1:10),c(1,7)])
#tiempo
#new deaths smoothed

ews_univariado_usa_3<- uniEWS(data = df_covid_usa[-c(1:10),c(1,7)] ,
                                metrics =  ews_metrics,
                                method = "expanding",
                                burn_in = 10,
                                threshold = 2,
                                tail.direction = "one.tailed")
plot(ews_univariado_usa_3)

pdf("03_out/plots/ews_univariado_usa.new.deaths.smoothed.pdf", height = 8, width = 10)
plot(ews_univariado_usa_3)
dev.off()

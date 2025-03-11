######PAISES CON MÁS PRUEBAS PT1.#####
#DINAMARCA
#ITALIA
#COREA
#INDIA
#UK

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

str(covid)
#FILTRADO:
covid_usa <- covid %>%filter(iso_code== "USA")

df_covid_usa <- data.frame(
  tiem = seq(1, length(covid_usa$date), 1) ,
  covid_usa[ ,5:16]
)

head(df_covid_usa)
names(df_covid_usa)

##VECTOR CON LAS METRICAS:
ews_metrics


##
head(df_covid_usa[rowSums(is.na(df_covid_usa)) == 0,]) ## para eliminar reglones con NA

################################################################################
names(df_covid_usa[-c(1:10),c(1,3)]) #para que solo este el tiempo
#y los nuevos casos. Lo necesario para realizar el analisis.
#tiempo
#nuevos casos.

##tiene datos faltantes: 
library(dplyr)
library(zoo)

summary(faltante_usa$new_cases) #tienen muchos NA :/
datos_usa_sin_na <- df_covid_usa[!is.na(df_covid_usa$new_cases), ] #NO NA

#interpolados:
faltante_usa <- df_covid_usa[-c(1:10),c(1,3)] #<- na.approx(df$value)
faltante_usa$new_cases_interpolate <- na.approx(faltante_usa$new_cases, rule = 2)
head(faltante_usa)


ews_univariado_usa<- uniEWS(data = faltante_usa[ ,c(1,3)], #datos interpolados; checar esto
                            #algunos datos pueden NO dar la manera correcta.
                               metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
                               method = "expanding", #VENTANA QUE SE EXPANDE
                               burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
                               threshold = 2, #VARIANZAS
                               tail.direction = "one.tailed") 
plot(ews_univariado_usa) #muy parecida a la de mexico
 
#datos_usa_sin_na

ews_univariado_usa_na<- uniEWS(data = datos_usa_sin_na[-c(1:10),c(1,3)], #datos interpolados; checar esto
                            #algunos datos pueden NO dar la manera correcta.
                            metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
                            method = "expanding", #VENTANA QUE SE EXPANDE
                            burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
                            threshold = 2, #VARIANZAS
                            tail.direction = "one.tailed") 
plot(ews_univariado_usa_na)

################################################################################
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

################################################################################


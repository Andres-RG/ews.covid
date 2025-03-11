######PAISES CON MÁS PRUEBAS PT1.#####
#COREA


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
covid_corea <- covid %>%filter(iso_code== "KOR")

df_covid_corea <- data.frame(
  tiem = seq(1, length(covid_corea$date), 1) ,
  covid_corea[ ,5:16]
)

names(df_covid_corea)

##VECTOR CON LAS METRICAS:
ews_metrics

################################################################################
names(df_covid_corea[-c(1:10),c(1,3)]) 

ews_univariado_corea <- uniEWS(data = df_covid_corea[-c(1:10),c(1,3)],
                               metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
                               method = "expanding", #VENTANA QUE SE EXPANDE
                               burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
                               threshold = 2, #VARIANZAS
                               tail.direction = "one.tailed") 
plot(ews_univariado_corea) 

################################################################################
names(df_covid_corea[-c(1:10),c(1,4)]) 
#time
#new cases smoothed

ews_univariado_corea_2 <- uniEWS(data = df_covid_corea[-c(1:10),c(1,4)] ,
                           metrics =  ews_metrics,
                           method = "expanding",
                           burn_in = 10,
                           threshold = 2,
                           tail.direction = "one.tailed")

plot(ews_univariado_corea_2)
##################################################################################
names(df_covid_corea[-c(1:10),c(1,7)])
#tiempo
#new deaths smoothed

ews_univariado_corea_3<- uniEWS(data = df_covid_corea[-c(1:10),c(1,7)] ,
                           metrics =  ews_metrics,
                           method = "expanding",
                           burn_in = 10,
                           threshold = 2,
                           tail.direction = "one.tailed")
plot(ews_univariado_corea_3)

##################################################################################

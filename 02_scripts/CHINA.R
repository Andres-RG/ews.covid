######PAISES CON MÁS PRUEBAS PT1.#####
#1.- FILTRADO DE DATOS:
#DATOS:
covid <- owid_covid() # de aqui se carga la base de datos de owid
str(covid)

#FILTRADO:
covid_china <- covid %>%filter(iso_code=="CHN")
#PARA GUARDAR LA BASE DE DATOS POR PAIS:
#save(objeto, "nombre del archivo.RData")

#UNIVARIADAS:
df_covid_china <- data.frame(
  tiem = seq(1, length(covid_china$date), 1) ,
  covid_china[ ,5:16]
)
names(df_covid_china)


#que siginifica cada columa:
# 3 -> new cases
# 4 -> new cases smoothed
#> ews univariate

#vector con las metricas:
ews_metrics <- c("SD","ar1","skew") #las de default!!!
###########
names(df_covid_china[-c(1:10),c(1,3)]) #para que solo este el tiempo
#y los nuevos casos. Lo necesario para realizar el analisis.
#tiempo
#nuevos casos.

ews_univariado_china <- uniEWS(data = df_covid_china[-c(1:10),c(1,3)],
      metrics =  ews_metrics, #VECTOR CON LAS METRICAS.
      method = "expanding", #VENTANA QUE SE EXPANDE
      burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
      threshold = 2, #VARIANZAS
      tail.direction = "one.tailed") 
plot(ews_univariado_china) 
# pdf("03_out/plots/ews_univariado_china.nuevos.casos.pdf", height = 8, width = 10)
# plot(ews_univariado_china)
# dev.off()

#############
names(df_covid_china[-c(1:10),c(1,4)])
#timpo
#nuevos casos suavizados.

ews_univariado_china_2<- uniEWS(data = df_covid_china[-c(1:10),c(1,4)] ,
                           metrics =  ews_metrics,
                           method = "expanding",
                           burn_in = 10,
                           threshold = 2,
                           tail.direction = "one.tailed")
plot(ews_univariado_china_2)
# pdf("03_out/plots/ews_univariado_china.nuevos.suavizado.pdf", height = 8, width = 10)
# plot(ews_univariado_china_2)
# dev.off()

###############
names(df_covid_china[-c(1:10),c(1,7)])
#tiempo
#nuevas muertes suavizadas

#tiene datos faltantes: intento de interpolar:
library(dplyr)
library(zoo)
# Interpolate missing values
faltante <- df_covid_china[-c(1:10),c(1,7)] #<- na.approx(df$value)
faltante$new_deaths_smoothed_interpolated <- na.approx(faltante$new_deaths_smoothed)

datos_china_sin_na <- df_covid_china[!is.na(df_covid_china$new_deaths_smoothed), ]

#funcion:
head(covid_usa[rowSums(is.na(covid_usa)) == 0,]) ## para eliminar reglones con NA

ews_univariado_china_3<- uniEWS(data = datos_china_sin_na [-c(1:10),c(1,7)] ,
                           metrics =  ews_metrics,
                          method = "expanding",
                         burn_in = 10,
                        threshold = 2,
                       tail.direction = "one.tailed")
plot(ews_univariado_china_3)

#CON LOS DATOS INTENTO INTERPOLADOS:
#estimacion de los valores desconocidos, basados en otros.

#ews_univariado_china_4<- uniEWS(data = faltante[ ,c(1,3)], #datos extrapolados
 #                               metrics =  ews_metrics,
  #                              method = "expanding",
   #                             burn_in = 10,
    #                            threshold = 2,
     #                           tail.direction = "one.tailed")
#plot(ews_univariado_china_4) 

####################################################################################
#intento multivariado :/
#1-tiempo
#2-nuevos casos
#8-total cases
#11-total cases per million


#ews_multi_china <- multiEWS(data = df_covid_china, #QUE MEDIDAS TOMAR.
 #                              metrics =  c("meanAR","maxAR","meanSD","maxSD","eigenMAF",
  #                                          "mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV",
   #                                         "mutINFO"),
    #                           method = "expanding", #VENTANA QUE SE EXPANDE
     #                          burn_in = 10, #DE PREFERENCIA CON ESTOS DATOS.
      #                         threshold = 2, #VARIANZAS
       #                        tail.direction = "one.tailed") 
#plot(ews_univariado_china) 

####################################################################################
#con los datos de la incidencia:
#checar que variables se utilizarian para este tipo de analisis.
#El objeto con los casos positivos: del modelo matematico.
## se obtiene la incidencia diaria: checar las variables de la incidencia

# se hace el data frame para calcular ews.
#la base de datos para realizar el conteo univariado.
data_frame_covid_for_ews <- data.frame(
  time = seq(1, length(casos_positivos_re_conteo$FECHA_SINTOMAS), 1) , #siempre tiene que ser TIEMPO
  casos = casos_positivos_re_conteo$positivos #SIEMPRE LOS CASOS
  #AUTOCORRELACION, DESVIACIONES Y LA COMBINACION.
)

# se obtiene la incidencia diaria
incidencia <- casos_positivos_re_conteo[, -3]
incidencia <- as.data.frame(incidencia)
colnames(incidencia) <- c("dates", "I")
head(incidencia)
##complementar las fechas
fechas_continuas <- seq(min(incidencia$dates), 
                        max(incidencia$dates), by = "1 day")
incidencia <- merge(data.frame(dates = fechas_continuas),
                    incidencia, by = "dates", all.x = TRUE)
incidencia$I[is.na(incidencia$I)] <- 0
head(incidencia)

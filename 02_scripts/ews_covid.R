# ews covid
##------------------------------------------
# se cargan los datos de modelo_matematico_covid
setwd("~/Documents/modelo_matematico_covid/") # modificar ruta donde este descargado
load("03_Out/OutData/casos_datos_x_grupos.RData")
casos_x_grupos
head(casos_x_grupos)
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
casos_positivos_re_conteo <- as.data.frame(casos_positivos_re_conteo)
head(head(casos_positivos_re_conteo))
# se cargan los datos de rt
setwd("~/Documents/maestria/ews.covid/") # modificar ruta donde este descargado
load("01_raw_data/rt_covid_qro_values.RData") #rdata
##------------------------------------------
# se cargan las librerias
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)
##------------------------------------------
# se cargan las funciones extra
source("04_functions/dataunvariateews.R")
source("04_functions/plotewsunivariateggplot.R")
##------------------------------------------
# se hace el data frame para calcular ews 
data_frame_covid_for_ews <- data.frame(
  time = seq(1, length(casos_positivos_re_conteo$FECHA_SINTOMAS), 1) ,
  casos = casos_positivos_re_conteo$positivos
)
##------------------------------------------
# ews univariados
ews_metrics <- c("SD","ar1","skew")
ews_exp_windo_covid <- uniEWS(data = data_frame_covid_for_ews,
                                    metrics =  ews_metrics,
                                    method = "expanding", 
                                    burn_in = 10, 
                                    threshold = 2,
                                    tail.direction = "one.tailed")
plot(ews_exp_windo_covid)
# pdf("03_out/plots/ews_univariate_covid_v2.pdf", height = 8, width = 10)
# plot(ews_exp_windo_covid)
# dev.off()
##
## grafica con ggplot
data_uni_ews_covid <- data.uni.ews(ews_exp_windo_covid)
ggplot_uni_ews_covid <- plot.univariate.ews.ggplot(datauniews = data_uni_ews_covid)
ggplot_uni_ews_covid <- ggplot_uni_ews_covid + 
  labs(title = "EWS for COVID")
ggplot_uni_ews_covid
# pdf("03_out/plots/ews_univariate_covid.pdf", height = 8, width = 10)
# ggplot_uni_ews_covid
# dev.off()
##------------------------------------------
# owid_covid()->X
# library(dplyr)
# X %>% filter(iso_code==“MEX”)->mexico
# ggplot(X, aes(x = date, y = new_cases_smoothed_per_million)) +
#   geom_point(
#     color = “darkred”,   # Point color
#     size = 3,            # Point size
#     alpha = 0.7          # Transparency (0-1)
#   ) +
#   labs(
#     title = “Example Scatter Plot”,
#     x = “Fecha”,
#     y = “Casos suavizados por millón”
#   ) +
#   theme_classic()

# covid <- owid_covid() # de aqui se carga la base de datos de owid
str(covid)
covid.mx <- covid %>% filter(iso_code=="MEX")
#> univariate data frame
df.covid.mx <- data.frame(
  time = seq(1, length(covid.mx$date), 1)     ,
  covid.mx[ , 5:16]
)
names(df.covid.mx)
# 3 -> new cases
# 4 -> new cases smoothed
#> ews univariate
ews_metrics <- c("SD","ar1","skew")
ews.uni.covid.mx1<- uniEWS(data = df.covid.mx[-c(1:10),c(1,3)] ,
                          metrics =  ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed")
plot(ews.uni.covid.mx1)
ews.uni.covid.mx2<- uniEWS(data = df.covid.mx[-c(1:10),c(1,4)] ,
                          metrics =  ews_metrics,
                          method = "expanding",
                          burn_in = 10,
                          threshold = 2,
                          tail.direction = "one.tailed")
plot(ews.uni.covid.mx2)
ews.uni.covid.mx3<- uniEWS(data = df.covid.mx[-c(1:10),c(1,7)] ,
                           metrics =  ews_metrics,
                           method = "expanding",
                           burn_in = 10,
                           threshold = 2,
                           tail.direction = "one.tailed")
plot(ews.uni.covid.mx3)
# pdf("03_out/plots/ews.covidmx.pdf", height = 8, width = 10)
# plot(ews.uni.covid.mx1)
# plot(ews.uni.covid.mx2)
# dev.off()

# para guarda en calidad alta
# tiff("03_out/plots/ews.covidmx.new.cases.tiff",
#      units = "in", width=8, height=6, res=300)
# plot(ews.uni.covid.mx1)
# dev.off()
##------------------------------------------
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
##
# con rt
head(rdata)
df_covid_for_ews <- data.frame(
  time = seq(1, length(incidencia$I[-c(587:593)]), 1 )   ,
  i = incidencia$I[-c(587:593)]                          ,
  r_t = rdata$`Mean(R)`                     
)

head(df_covid_for_ews)
ews_multivariate_covid_qro <- multiEWS(data= df_covid_for_ews,
         method = "expanding",
         burn_in = 10)
plot(ews_multivariate_covid_qro)

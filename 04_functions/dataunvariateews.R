## Funcion para extraer las metricas de EWS univariadas a un dataframe para 
## hacer la grafica en ggplot
data.uni.ews <- function( objetoewsuni ){
  data_uni_ews <- data.frame(
    # se define el tiempo
    time = unique ( objetoewsuni$EWS$time )                                   ,
    # ar1
    ar1 = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "ar1" ]      ,
    threshold.crossed.ar1 = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "ar1" ] ,
    # SD
    SD = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "SD" ]      ,
    threshold.crossed.SD = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "SD" ] ,
    # skew
    skew = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "skew" ]      ,
    threshold.crossed.skew = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "skew" ] ,
    # ar1 + SD
    ar1.SD = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "ar1 + SD" ]      ,
    threshold.crossed.ar1.SD = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "ar1 + SD" ] ,
    # ar1 + skew
    ar1.skew = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "ar1 + skew" ]      ,
    threshold.crossed.ar1.skew = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "ar1 + skew" ] ,
    # SD + skew
    SD.skew = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "SD + skew"]      ,
    threshold.crossed.SD.skew = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "SD + skew" ] ,
    # ar1 + SD + skew
    ar1.SD.skew = objetoewsuni$EWS$str [ objetoewsuni$EWS$metric.code == "ar1 + SD + skew"]      ,
    threshold.crossed.ar1.SD.skew = objetoewsuni$EWS$threshold.crossed [ objetoewsuni$EWS$metric.code == "ar1 + SD + skew" ]
  )
}
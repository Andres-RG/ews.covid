## Funcion para extraer las metricas de EWS multivariadas a un data frame para
## hacer la grafica con ggplot.

data.ews <- function(objetoews){
  require(dplyr)
  require(tidyr)
  require(tidyverse)
  
  data_ews <- objetoews$EWS$raw %>%
    filter(metric.code %in% c("eigenCOV",
                              "eigenMAF",
                              "mafAR"   ,
                              "mafSD"   ,
                              "maxAR"   ,
                              "maxCOV"  ,
                              "maxSD"   ,
                              "meanAR"  ,
                              "meanSD"  ,
                              "mutINFO" ,
                              "pcaAR"   ,
                              "pcaSD"   )) %>%
    select(time, metric.code, str, threshold.crossed) %>%
    pivot_wider(names_from = metric.code,
                values_from = c(str, threshold.crossed))
  colnames(data_ews) <- c("time"    ,
                          "eigenCOV",
                          "eigenMAF",
                          "mafAR"   ,
                          "mafSD"   ,
                          "maxAR"   ,
                          "maxCOV"  ,
                          "maxSD"   ,
                          "meanAR"  ,
                          "meanSD"  ,
                          "mutINFO" ,
                          "pcaAR"   ,
                          "pcaSD"   ,
                          "threshold.crossed.eigenCOV",
                          "threshold.crossed.eigenMAF",
                          "threshold.crossed.mafAR",
                          "threshold.crossed.mafSD",
                          "threshold.crossed.maxAR",
                          "threshold.crossed.maxCOV",
                          "threshold.crossed.maxSD",
                          "threshold.crossed.meanAR",
                          "threshold.crossed.meanSD" ,
                          "threshold.crossed.mutINFO",
                          "threshold.crossed.pcaAR",
                          "threshold.crossed.pcaSD")
  return(data_ews)
}
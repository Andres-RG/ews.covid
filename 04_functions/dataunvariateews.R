## Funcion para extraer las metricas de EWS univariadas a un dataframe para 
## hacer la grafica en ggplot
data.uni.ews <- function( ewsuni ){
  require(dplyr)
  require(tidyr)
  require(tidyverse)
  
  data.uni.ews <- ewsuni$EWS %>%
    filter(metric.code %in% c("ar1", 
                              "SD",
                              "skew",
                              "ar1 + SD",
                              "ar1 + skew",
                              "SD + skew",
                              "ar1 + SD + skew")) %>%
    select(time, metric.code, str, threshold.crossed) %>%
    pivot_wider(names_from = metric.code, values_from = c(str, threshold.crossed))
  colnames(data.uni.ews) <- c("time",
                              "ar1", "SD", "skew", "ar1.SD", "ar1.skew",
                              "SD.skew", "ar1.SD.skew",
                              "threshold.crossed.ar1",
                              "threshold.crossed.SD",
                              "threshold.crossed.skew",
                              "threshold.crossed.ar1.SD",
                              "threshold.crossed.ar1.skew",
                              "threshold.crossed.SD.skew",
                              "threshold.crossed.ar1.SD.skew")
  return(data.uni.ews)
}

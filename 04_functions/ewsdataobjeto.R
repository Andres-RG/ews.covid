#Data frame y objeto EWS para el analisis.
##------------------------------------------------------------------------------
ews_analisis <-function(incidencia){
  ewsdatos_ <- data.frame(
    time = seq(1, length(incidencia$FECHA_SINTOMAS), 1) ,
    casos = incidencia$positivos)
  
  ews_metrics <- c("SD","ar1","skew")
  
  analisis_ews <- uniEWS(data = ewsdatos_,
                         metrics = ews_metrics,
                         method = "expanding",
                         burn_in = 10,
                         threshold = 2,
                         tail.direction = "one.tailed"
  )
  return(analisis_ews)
}
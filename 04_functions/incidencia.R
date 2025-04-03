#funcion incidencia diaria.
avance <- function(datos){
  bc_20 <- c()
  for (i in 1:length(datos$FECHA_SINTOMAS)) {
    bc_20 <-c(bc_20, 1)
  }
  positivos_re_ <- mutate(datos, positivos = bc_20) 
  positivos_re_ <- aggregate(positivos~FECHA_SINTOMAS, 
                             data = positivos_re_,
                             FUN = sum)
  positivos_re_[,3] <- c(1:length(positivos_re_$FECHA_SINTOMAS))
  colnames(positivos_re_)[3] <- "num.dia" 
  
  return(positivos_re_)
  
}
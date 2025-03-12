#CDMX:
load("03_out/data/covid.mx.cdmx.RData")
head(covid.mx.cdmx)
# covid.mx.cdmx$CLASIFICACION_FINAL_COVID: 3
covid.mx.cdmx.positivos <- covid.mx.cdmx[which(covid.mx.cdmx$CLASIFICACION_FINAL_COVID == "3"), ]
View(covid.mx.cdmx.positivos) #Casos positivos por muestra de PCR. 

################################################################################
casos_positivos_re <- filter(covid.mx.cdmx, CLASIFICACION_FINAL_COVID  == 1 |
                               CLASIFICACION_FINAL_COVID  == 2 |
                               CLASIFICACION_FINAL_COVID  == 3 )

# 8. Casos por dia. Incidencia =================================================

pos <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re$FECHA_SINTOMAS) ) {
  pos <- c(pos, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_re_conteo <- mutate(casos_positivos_re, positivos = pos) 
# genera una nueva columna en la base de los casos_positivos_re
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos 
# los renglones. 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_re_conteo <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_re_conteo,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_re_conteo [,3] <- c(1:length(casos_positivos_re_conteo$FECHA_SINTOMAS))
colnames(casos_positivos_re_conteo)[3] <- "num.dia" 
casos_positivos_re_conteo


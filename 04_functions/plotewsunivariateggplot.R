## Funcion para hacer graficas utilizando ggplot a partir de un objeto data.ews
## de metricas univariadas

plot.univariate.ews.ggplot <- function( datauniews ){
  # se llama la libreria
  require(ggplot2)
  #
  ggplot(data = datauniews,
         aes(x = time)) +
    # ar1
    geom_line(aes(y = ar1,
                  col = "ar1",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.ar1 == 1),
               aes(y = ar1,
                   shape = "Detected"),
               col = "#6886c4",
               size = 2) +
    # SD
    geom_line(aes(y = SD,
                  col = "SD",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.SD == 1),
               aes(y = SD,
                   shape = "Detected"),
               col = "#bfbd3d",
               size = 2) +
    # skew
    geom_line(aes(y = skew,
                  col = "skew",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.skew == 1),
               aes(y = skew,
                   shape = "Detected"),
               col = "#5d3099",
               size = 2) +
    # ar1 + SD
    geom_line(aes(y = ar1.SD,
                  col = "ar1.SD",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.ar1.SD == 1),
               aes(y = ar1.SD,
                   shape = "ar1.SD"),
               col = "#69c756",
               size = 2) +
    # ar1 + skew
    geom_line(aes(y = ar1.skew,
                  col = "ar1.skew",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.ar1.skew == 1),
               aes(y = ar1.skew,
                   shape = "ar1.skew"),
               col = "#e281fe",
               size = 2) +
    # SD + skew
    geom_line(aes(y = SD.skew,
                  col = "SD.skew",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.SD.skew == 1),
               aes(y = SD.skew,
                   shape = "SD.skew"),
               col = "#6ca181",
               size = 2) + 
    # ar1 + SD + skew
    geom_line(aes(y = ar1.SD.skew,
                  col = "ar1.SD.skew",
                  linetype = "Undetected"),
              lwd = 0.6) +
    geom_point(data = subset( datauniews,
                              threshold.crossed.ar1.SD.skew == 1),
               aes(y = ar1.SD.skew,
                   shape = "ar1.SD.skew"),
               col = "#76c3ef",
               size = 2) +
    ###
    scale_shape_manual(values = c("Detected" = 16)) +
    scale_linetype_manual(values = c("Undetected" = "solid")) +
    ###
    scale_color_manual(values = c("ar1" = "#6886c4",
                                  "SD" = "#bfbd3d",
                                  "skew"    = "#5d3099",
                                  "ar1.SD"    = "#69c756",
                                  "ar1.skew"    = "#e281fe",
                                  "SD.skew"   = "#6ca181",
                                  "ar1.SD.skew"    = "#76c3ef")) +
    ###
    labs(x = "Time", y = "Strength of EWS",
         color = "Univariate EWS
indicator strength",
         shape = "Detected EWS",
         linetype = "Undetected EWS") +
    ###
    guides(shape = guide_legend(override.aes = list(col = "black")),
           linetype = guide_legend()) +
    ###
    theme(
      # title
      plot.title = element_text(size = 12,
                                face = "bold"),
      # linea del eje
      axis.line = element_line(colour = "black",
                               linewidth = 0.3),
      # eje x
      axis.text.x = element_text(angle = 0,
                                 hjust = 1,
                                 face = "bold"),
      axis.title.x = element_text(size = 11,
                                  face = "bold"),
      # eje y
      axis.text.y = element_text(size = 9,
                                 face = "bold"),
      axis.title.y = element_text(size = 11,
                                  face = "bold"),
      # leyenda
      legend.position = "right",  # Posición de la leyenda
      legend.title = element_text(size = 10,
                                  face = "bold"),  # Título de la leyenda
      legend.text = element_text(size = 10),  # Texto de la leyenda
      legend.spacing = unit(0.5, "cm"),
      legend.background = element_rect(fill = "white"),  # Fondo blanco con borde negro
      # fondo
      panel.background = element_rect(fill = "white"), # Fondo blanco
      panel.grid.major.y = element_blank(), # Elimina la cuadrícula mayor del eje Y
      panel.grid.minor.y = element_blank(), # Elimina la cuadrícula menor del eje Y
      panel.grid.major.x = element_blank()) +
    #
    geom_hline(yintercept = c(2, 0, -2), # Líneas punteadas en el eje Y
               linetype = "dashed",
               color = "grey40",
               lwd = 0.7)
}
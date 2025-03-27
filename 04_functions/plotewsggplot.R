## Funcion para hacer graficas utilizando ggplot a partir de un objeto data.ews

plot.ews.ggplot <- function(objetoews){
  
  # se llama a la libreria
  require(ggplot2)
  #
  ggplot(data = objetoews,
         aes(x = time)) +
  ### eigenCOV
  geom_line(aes(y = eigenCOV,
                col = "eigenCOV",
                linetype = "Undetected"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.eigenCOV == 1),
             aes(y = eigenCOV,
                 shape = "Detected"),
             col = "#6886c4",
             size = 2) +
  ### eigenMAF
  geom_line(aes(y = eigenMAF,
                col = "eigenMAF"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.eigenMAF == 1),
             aes(y = eigenMAF,
                 shape = "Detected"),
             col = "#bfbd3d",
             size = 2) +
  ### mafAR
  geom_line(aes(y = mafAR,
                col = "mafAR"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.mafAR == 1),
             aes(y = mafAR,
                 shape = "Detected"),
             col = "#5d3099",
             size = 2) +
  ### mafSD
  geom_line(aes(y = mafSD,
                col = "mafSD"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.mafSD == 1),
             aes(y = mafSD,
                 shape = "Detected"),
             col = "#69c756",
             size = 2) +
  ### maxAR
  geom_line(aes(y = maxAR,
                col = "maxAR"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.maxAR == 1),
             aes(y = maxAR,
                 shape = "Detected"),
             col = "#e281fe",
             size = 2) +
  ### maxCOV
  geom_line(aes(y = maxCOV,
                col = "maxCOV"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.maxCOV == 1),
             aes(y = maxCOV,
                 shape = "Detected"),
             col = "#6ca181",
             size = 2) +
  ### maxSD
  geom_line(aes(y = maxSD,
                col = "maxSD"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.maxSD == 1),
             aes(y = maxSD,
                 shape = "Detected"),
             col = "#76c3ef",
             size = 2) +
  ### meanAR
  geom_line(aes(y = meanAR,
                col = "meanAR"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.meanAR == 1),
             aes(y = meanAR,
                 shape = "Detected"),
             col = "#d06329",
             size = 2) +
  ### meanSD
  geom_line(aes(y = meanSD,
                col = "meanSD"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.meanSD == 1),
             aes(y = meanSD,
                 shape = "Detected"),
             col = "#90676f",
             size = 2) +
  ### mutINFO
  geom_line(aes(y = mutINFO,
                col = "mutINFO"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.mutINFO == 1),
             aes(y = mutINFO,
                 shape = "Detected"),
             col = "#ce5c6e",
             size = 2) +
  ### pcaAR
  geom_line(aes(y = pcaAR,
                col = "pcaAR"),
            lwd = 0.6) + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.pcaAR == 1),
             aes(y = pcaAR,
                 shape = "Detected"),
             col = "#5d4216",
             size = 2) +
  ### pcaSD
  geom_line(aes(y = pcaSD,
                col = "pcaSD"),
            lwd = 0.6,
            linetype = "dashed") + 
  geom_point(data = subset(objetoews,
                           threshold.crossed.pcaSD == 1),
             aes(y = pcaSD,
                 shape = "Detected"),
             col = "black",
             size = 2) +
  ###
  scale_shape_manual(values = c("Detected" = 16)) +
  scale_linetype_manual(values = c("Undetected" = "solid")) +
  ###
  scale_color_manual(values = c("eigenCOV" = "#6886c4",
                                "eigenMAF" = "#bfbd3d",
                                "mafAR"    = "#5d3099",
                                "mafSD"    = "#69c756",
                                "maxAR"    = "#e281fe",
                                "maxCOV"   = "#6ca181",
                                "maxSD"    = "#76c3ef",
                                "meanAR"   = "#d06329",
                                "meanSD"   = "#90676f",
                                "mutINFO"  = "#ce5c6e",
                                "pcaAR"    = "#5d4216",
                                "pcaSD"    = "black")) +
  ###
  labs(x = "Time", y = "Strength of EWS",
       color = "Multivariate EWS
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
  geom_hline(yintercept = c(2), # Líneas punteadas en el eje Y
             linetype = "dashed",
             color = "grey40",
             lwd = 0.7)
}

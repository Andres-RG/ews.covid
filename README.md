# 📈 ews.covid  

**Detección de Early Warning Signals en los datos de COVID-19**  

Este repositorio implementa análisis de señales tempranas (*Early Warning Signals*) en los datos de COVID-19 utilizando el paquete `EWSmethods`.  

## 📌 Requisitos  
Para usar este repositorio, necesitas tener descargado o conectado el siguiente repositorio en GitHub:  
🔗 [modelo_matematico_covid](https://github.com/Andres-RG/modelo_matematico_covid)  

## 📦 Instalación de paquetes necesarios  
Asegúrate de tener instaladas y cargadas las siguientes librerías en R:  

```r
install.packages(c("tidyr", "tidyverse", "ggplot2", "dplyr"))
# Para paquetes de CRAN
install.packages("owidR", repos = "https://cran.r-project.org")
# Para instalar EWSmethods desde GitHub (si es necesario)
remotes::install_github("earlywarningtoolbox/EWSmethods")

# Cargar librerías
library(tidyr)
library(tidyverse)
library(EWSmethods)
library(ggplot2)
library(owidR)
library(dplyr)
```

## 📂 Estructura del repositorio
📁 01_raw_data → Datos crudos de COVID-19.
📁 02_scripts → Scripts de análisis y procesamiento.
📁 03_out/plots → Gráficos generados del análisis.
📁 04_functions → Funciones personalizadas para el análisis.

## 🚀 Uso
Para ejecutar el análisis, clona el repositorio y corre los scripts en 02_scripts.

```bash
git clone https://github.com/Andres-RG/ews.covid.git
```

Luego, abre los scripts en RStudio y ejecútalos en orden.

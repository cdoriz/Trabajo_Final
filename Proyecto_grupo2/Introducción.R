#Introducción

# Descargar la carpeta Proyecto_grupo2 con sus respectivos archivos guardados en el orden correspondiente
# Ejecutar el siguiente código:

# Librerias
library(here)
library(readr)

# Ruta al archivo
BlackFriday_gasto <- here("data", "raw", "BlackFriday_con_gasto.csv")

# Cargar el dataset
BlackFriday <- read_csv(BlackFriday_gasto)

# Ir a la carpeta scripts y correr los codigos en el sigueinte orden:
  # 01_Observar_Variables
  # 02_NA_Outliers
  # 03_Antes_Despues
  # 04_Estadistica
  # 05_Plots

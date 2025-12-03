# Introducción

# Librerias
library(here)
library(readr)

# Ruta al archivo
BlackFriday_gasto <- here("data", "raw", "BlackFriday_con_gasto.csv")

# Cargar el dataset
BlackFriday <- read_csv(BlackFriday_gasto)

# Luego ir a la carpeta scripts y correr los siguientes codigos
  # 01_Observar_Variables
  # 02_NA_Outliers
  # 03_Antes_Despues
  # 04_Estadistica
  # 05_Plots
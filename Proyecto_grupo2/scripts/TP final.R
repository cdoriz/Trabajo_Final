# Librerias
library(here)
library(tidyverse)
library(car)
library(lmtest)
library(sandwich)
library(broom)
library(scales)

# Ruta al archivo en data/raw
BlackFriday <- here("data", "raw", "BlackFriday.csv")

# Cargar el dataset
BlackFriday <- read_csv(BlackFriday)
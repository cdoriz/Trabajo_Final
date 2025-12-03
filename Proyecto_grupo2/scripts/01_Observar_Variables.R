# 01 observar las variables

# Librerias
library(here)
library(tidyverse)
library(readr)
library(dplyr)

# Tipos y conversiones iniciales
BlackFriday <- BlackFriday %>%
  mutate(
    Gender = as.factor(Gender),
    Age = as.factor(Age),
    City_Category = as.factor(City_Category),
    Stay_In_Current_City_Years = as.factor(Stay_In_Current_City_Years),
    Product_Category_1 = as.factor(Product_Category_1),
    Product_Category_2 = as.factor(Product_Category_2),
    Product_Category_3 = as.factor(Product_Category_3)
    )

# NA por variable
na_total <- BlackFriday %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_na") %>%
  mutate(
    pct_na = round(n_na / nrow(BlackFriday) * 100, 3)
  )

write_csv(na_total, here("data/clean/01_na_summary.csv"))
# hay NA en Product_Category_2 y Product_Category_3

# Estadísticas descriptivas del Gasto
analisis_gasto <- function(base){
  base %>%
    summarise(
      n = n(),
      media = mean(Purchase, na.rm = TRUE),
      mediana = median(Purchase, na.rm = TRUE),
      sd = sd(Purchase, na.rm = TRUE),
      min = min(Purchase, na.rm = TRUE),
      max = max(Purchase, na.rm = TRUE),
      q1 = quantile(Purchase, 0.25, na.rm = TRUE),
      q3 = quantile(Purchase, 0.75, na.rm = TRUE),
      iqr = IQR(Purchase, na.rm = TRUE)
    )
}

gasto_stats <- analisis_gasto(BlackFriday)

write_csv(gasto_stats,
          here("data/clean/01_gasto_descriptive_stats.csv"))

# Por genero,edad y ciudad
resumen_por_categoria <- function(base, opcion){
  base %>% 
    group_by({{ opcion }}) %>%
    summarise(
      gasto_promedio = mean(Purchase, na.rm = TRUE),
      n = n(),
      pct = round(n() / nrow(base) * 100, 2),
      .groups = "drop"
    )
}

gender_dist <- resumen_por_categoria(BlackFriday, Gender)
age_dist    <- resumen_por_categoria(BlackFriday, Age)
city_dist   <- resumen_por_categoria(BlackFriday, City_Category)

write_csv(gender_dist, here("data/clean/01_distribucion_genero.csv"))
write_csv(age_dist,    here("data/clean/01_distribucion_age.csv"))
write_csv(city_dist,   here("data/clean/01_distribucion_ciudad.csv"))

# Guardar base
saveRDS(BlackFriday,
        here("data/processed/BlackFriday_01.rds"))
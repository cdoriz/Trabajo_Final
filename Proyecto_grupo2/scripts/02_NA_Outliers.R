# 02 NA y Outliers

# Librerias
library(here)
library(readr)
library(dplyr)

BlackFriday <- readRDS("data/processed/BlackFriday_01.rds")

# Resumen de NAs por variable que esta en el script 01
print(na_total)

total_obs <- nrow(BlackFriday)

# Análisis específico de Product_Category_2 y 3
cat("\n--- Análisis de categorías de productos faltantes ---\n")
cat("Product_Category_2 - NAs:", sum(is.na(BlackFriday$Product_Category_2)), 
    "(", round(sum(is.na(BlackFriday$Product_Category_2))/total_obs*100, 2), "%)\n")
cat("Product_Category_3 - NAs:", sum(is.na(BlackFriday$Product_Category_3)), 
    "(", round(sum(is.na(BlackFriday$Product_Category_3))/total_obs*100, 2), "%)\n")

# Patrón de combinación de NAs
patron_nas <- BlackFriday %>%
  mutate(
    cat2_na = is.na(Product_Category_2),
    cat3_na = is.na(Product_Category_3)
  ) %>%
  group_by(cat2_na, cat3_na) %>%
  summarise(
    n = n(),
    pct = round(n() / total_obs * 100, 2),
    .groups = "drop"
  )

# Verificar si hay patrón sistemático en los NAs
nas_por_genero <- BlackFriday %>%
  group_by(Gender) %>%
  summarise(
    total = n(),
    cat2_na = sum(is.na(Product_Category_2)),
    cat3_na = sum(is.na(Product_Category_3)),
    pct_cat2_na = round(cat2_na / total * 100, 2),
    pct_cat3_na = round(cat3_na / total * 100, 2),
    .groups = "drop"
  )

nas_por_edad <- BlackFriday %>%
  group_by(Age) %>%
  summarise(
    total = n(),
    cat2_na = sum(is.na(Product_Category_2)),
    cat3_na = sum(is.na(Product_Category_3)),
    pct_cat2_na = round(cat2_na / total * 100, 2),
    pct_cat3_na = round(cat3_na / total * 100, 2),
    .groups = "drop"
  )

nas_por_ciudad <- BlackFriday %>%
  group_by(City_Category) %>%
  summarise(
    total = n(),
    cat2_na = sum(is.na(Product_Category_2)),
    cat3_na = sum(is.na(Product_Category_3)),
    pct_cat2_na = round(cat2_na / total * 100, 2),
    pct_cat3_na = round(cat3_na / total * 100, 2),
    .groups = "drop"
  )


# Guardar análisis
write_csv(patron_nas, here("data", "clean", "02_patron_nas_categorias.csv"))
write_csv(nas_por_genero, here("data", "clean", "02_nas_por_genero.csv"))
write_csv(nas_por_edad, here("data", "clean", "02_nas_por_edad.csv"))
write_csv(nas_por_ciudad, here("data", "clean", "02_nas_por_ciudad.csv"))

# Outliners
# Estadísticas antes de detección
print(gasto_stats)

# IQR (Rango Intercuartílico)
q1 <- quantile(BlackFriday$Purchase, 0.25, na.rm = TRUE)
q3 <- quantile(BlackFriday$Purchase, 0.75, na.rm = TRUE)
iqr <- IQR(BlackFriday$Purchase, na.rm = TRUE)

limite_inferior <- q1 - 1.5 * iqr
limite_superior <- q3 + 1.5 * iqr

BlackFriday01 <- BlackFriday %>%
  mutate(
    is_outlier_iqr = Purchase < limite_inferior | Purchase > limite_superior
  )

n_outliers <- sum(BlackFriday01$is_outlier_iqr)
pct_outliers <- round(n_outliers / total_obs * 100, 2)

# Estadísticas de los outliers
outliers_stats <- BlackFriday01 %>%
  filter(is_outlier_iqr) %>%
  summarise(
    n_outliers = n(),
    purchase_min = min(Purchase),
    purchase_max = max(Purchase),
    purchase_mean = mean(Purchase),
    purchase_median = median(Purchase)
  )

# Z-score (valores > 3 desviaciones estándar)
BlackFriday01 <- BlackFriday01 %>%
  mutate(
    z_score = as.numeric(scale(Purchase)),
    is_outlier_z = ifelse(is.na(z_score), FALSE, abs(z_score) > 3)
    )

n_outliers_z <- sum(BlackFriday01$is_outlier_z)
pct_outliers_z <- round(n_outliers_z / total_obs * 100, 2)

# Guardar outliers
outliers_df <- BlackFriday01 %>%
  select(User_ID, Product_ID, Purchase, is_outlier_iqr) %>%
  arrange(desc(Purchase))

write_csv(outliers_df, here("data", "clean", "02_outliers_identificados.csv"))

saveRDS(BlackFriday01, here("data/processed/BlackFriday_02.rds"))
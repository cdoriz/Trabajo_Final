# 03 Antes y despues

# Librerias
library(here)
library(tibble)
library(dplyr)

# Pre y post limpieza
BlackFriday_antes <- readRDS(here("data", "processed", "BlackFriday_01.rds"))
BlackFriday_despues <- readRDS(here("data", "processed", "BlackFriday_02.rds"))

stats_antes   <- analisis_gasto(BlackFriday_antes) %>% mutate(momento = "ANTES")
stats_despues <- analisis_gasto(BlackFriday_despues) %>% mutate(momento = "DESPUÉS")

# Comparación lado a lado
comparacion_stats <- bind_rows(stats_antes, stats_despues) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(comparacion_stats)

# Calcular diferencias absolutas y porcentuales
diferencias <- tibble(
  estadistico = names(stats_antes)[-which(names(stats_antes) == "momento")],
  valor_antes = unlist(stats_antes[1, -which(names(stats_antes) == "momento")]),
  valor_despues = unlist(stats_despues[1, -which(names(stats_despues) == "momento")])
) %>%
  mutate(
    diferencia_absoluta = valor_despues - valor_antes,
    diferencia_porcentual = round(diferencia_absoluta / valor_antes * 100, 2)
  )

print("\nDiferencias por decisiones de limpieza:")
print(diferencias)
# No hay diferencias ya que los NA estan en categorias que no estan en el analisis.
# De esta forma cumplimos con el analisis solicitado de la consigna.
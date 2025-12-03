# 04 Estadistica

# Librerias
library(here)
library(readr)
library(dplyr)
library(tibble)
library(car)
library(broom)
library(lmtest)
library(sandwich)

options(scipen = 999)

BlackFriday <- readRDS("data/processed/BlackFriday_02.rds")

# Contrastes necesarios para ANOVA Type III
options(contrasts = c("contr.sum", "contr.poly"))

# Verificamos supuestos
# Normalidad
resumen_normalidad <- BlackFriday %>% 
  summarise(
    skew = mean((Purchase - mean(Purchase))^3) / sd(Purchase)^3,
    kurt = mean((Purchase - mean(Purchase))^4) / sd(Purchase)^4
  )
skew  <- resumen_normalidad$skew
kurt  <- resumen_normalidad$kurt

set.seed(123)
muestra <- sample_n(BlackFriday, min(5000, nrow(BlackFriday)))
shapiro_test <- shapiro.test(muestra$Purchase)

normalidad_df <- tibble(
  estadistico = c("Skewness", "Kurtosis", "Shapiro p-value"),
  valor = c(skew, kurt, shapiro_test$p.value),
  interpretacion = c(
    ifelse(abs(skew) < 0.5, "Simétrica", 
           ifelse(abs(skew) < 1, "Asimetría moderada", "Asimetría alta")),
    ifelse(abs(kurt - 3) < 1, "Normal", 
           ifelse(kurt > 3, "Colas pesadas (leptocúrtica)", "Colas ligeras (platicúrtica)")),
    ifelse(shapiro_test$p.value > 0.05, "No se rechaza normalidad", "Se rechaza normalidad")
  ),
  cumple_normalidad = c(
    ifelse(abs(skew) < 1, "Aceptable", "NO cumple"),
    ifelse(abs(kurt - 3) < 2, "Aceptable", "NO cumple"),
    ifelse(shapiro_test$p.value > 0.05, "Sí", "NO")
  )
)

write_csv(normalidad_df, here("output", "tables", "04_normalidad_purchase.csv"))
print(normalidad_df)

# Homogeneidad de varianzas
levene_genero <- leveneTest(Purchase ~ Gender, data = BlackFriday)
levene_edad <- leveneTest(Purchase ~ Age, data = BlackFriday)
levene_ciudad <- leveneTest(Purchase ~ City_Category, data = BlackFriday)
levene_product_C1 <- leveneTest(Purchase ~ Product_Category_1, data = BlackFriday)

levene_results <- tibble(
  variable = c("Gender", "Age", "City_Category", "Product_Category_1"),
  p_value = c(levene_genero$`Pr(>F)`[1], 
              levene_edad$`Pr(>F)`[1], 
              levene_ciudad$`Pr(>F)`[1],
              levene_product_C1$`Pr(>F)`[1])
) %>%
  mutate(
    cumple_homocedasticidad = ifelse(p_value > 0.05, "Sí", "NO")
  )

write_csv(levene_results, here("output", "tables", "04_levene_test_results.csv"))

# Test no parametricos
wilcox_result <- wilcox.test(Purchase ~ Gender, data = BlackFriday)
kruskal_age <- kruskal.test(Purchase ~ Age, data = BlackFriday)
kruskal_city <- kruskal.test(Purchase ~ City_Category, data = BlackFriday)
kruskal_product_C1 <- kruskal.test(Purchase ~ Product_Category_1, data = BlackFriday)

tests_no_parametricos <- tibble(
  test = c("Wilcoxon (Gender)", "Kruskal-Wallis (Age)", 
           "Kruskal-Wallis (City)", "Kruskal-Wallis (product_C1)"),
  statistic = c(wilcox_result$statistic, kruskal_age$statistic, 
                kruskal_city$statistic, kruskal_product_C1$statistic),
  p_value = c(wilcox_result$p.value, kruskal_age$p.value, 
              kruskal_city$p.value, kruskal_product_C1$p.value),
  significativo = ifelse(p_value < 0.05, "Sí", "No")
)

write_csv(tests_no_parametricos, here("output", "tables", "04_tests_no_parametricos.csv"))

# Modelo base (sin Product_Category_1)
modelo_base <- lm(Purchase ~ Gender + Age + City_Category, data = BlackFriday)
summary(modelo_base)

coefs_base_df <- tidy(modelo_base) %>%
  mutate(
    significativo = ifelse(p.value < 0.05, "Sí", "No")
  )

write_csv(coefs_base_df, here("output","tables","04_coeficientes_modelo_base.csv"))

# Métricas de bondad de ajuste - modelo base
bondadaj_base <- glance(modelo_base) %>%
  select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value) %>%
  mutate(modelo = "Base (sin Product_Category_1)")

write_csv(bondadaj_base, here("output", "tables", "04_bondad_ajuste_modelo_base.csv"))

# Modelo con Product_Category_1
modelo_completo <- lm(Purchase ~ Gender + Age + City_Category + Product_Category_1, data = BlackFriday)
summary(modelo_completo)

coefs_completo_df <- tidy(modelo_completo) %>%
  mutate(
    significativo = ifelse(p.value < 0.05, "Sí", "No")
  )

write_csv(coefs_completo_df, here("output", "tables", "04_coeficientes_modelo_completo.csv"))

# Métricas de bondad de ajuste - MODELO COMPLETO
bondadaj_completo <- glance(modelo_completo) %>%
  select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value) %>%
  mutate(modelo = "Completo (con Product_Category_1)")

write_csv(bondadaj_completo, here("output", "tables", "04_bondad_ajuste_modelo_completo.csv"))

# Comparo los modelos
comparacion_modelos <- bind_rows(bondadaj_base, bondadaj_completo) %>%
  mutate(across(where(is.numeric), ~round(., 6)))

write_csv(comparacion_modelos, here("output", "tables", "04_comparacion_modelos.csv"))

cat("\n=== RESUMEN COMPARATIVO DE MODELOS ===\n")
print(comparacion_modelos)
cat("\nMejora en R²:", 
    round((bondadaj_completo$r.squared - bondadaj_base$r.squared) * 100, 4), "%\n")

# VIF para detectar multicolinealidad - modelo completo
vif_vals <- vif(modelo_completo)

# Verificar si es matriz (variables categóricas) o vector
if(is.matrix(vif_vals)) {
  # Para variables categóricas
  vif_df <- tibble(
    variable = rownames(vif_vals),
    GVIF = vif_vals[, "GVIF"],
    Df = vif_vals[, "Df"],
    GVIF_ajustado = vif_vals[, "GVIF^(1/(2*Df))"],
    problema_multicolinealidad = case_when(
      GVIF_ajustado > 10 ~ "SEVERO",
      GVIF_ajustado > 5 ~ "Sí - Revisar",
      TRUE ~ "NO"
    )
  )
} else {
  # Para variables numéricas simples
  vif_df <- tibble(
    variable = names(vif_vals),
    VIF = as.numeric(vif_vals),
    problema_multicolinealidad = case_when(
      VIF > 10 ~ "SEVERO",
      VIF > 5 ~ "Sí - Revisar",
      TRUE ~ "NO"
    )
  )
}

write_csv(vif_df, here("output", "tables", "04_vif_modelo_completo.csv"))

# Heterocedasticidad - modelo completo
bp_test <- bptest(modelo_completo)
print(bp_test)

bp_df <- tibble(
  test = "Breusch-Pagan",
  statistic = bp_test$statistic,
  p_value = bp_test$p.value,
  cumple_homocedasticidad = ifelse(bp_test$p.value > 0.05, "Sí", "NO"),
  interpretacion = ifelse(bp_test$p.value > 0.05,
                          "No se detecta heterocedasticidad",
                          "Se detecta heterocedasticidad - usar errores robustos")
)

write_csv(bp_df, here("output", "tables", "04_breusch_pagan_test.csv"))

# Errores robustos
if(bp_test$p.value < 0.05) {
  coefs_robust <- coeftest(modelo_completo, vcov = vcovHC(modelo_completo, type = "HC1"))
  coefs_robust_df <- tidy(coefs_robust) %>%
    mutate(
      significativo = ifelse(p.value < 0.05, "Sí", "NO"),
      interpretacion = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )
  
  write_csv(coefs_robust_df, here("output", "tables", "04_coefs_robustos.csv"))
}

# ANOVA robusta HC3 (corrige heterocedasticidad)
vcov_hc3 <- sandwich::vcovHC(modelo_completo, type = "HC3")

anova_robusta <- car::Anova(modelo_completo, type = 3, vcov = vcov_hc3)

anova_robusta_df <- broom::tidy(anova_robusta) %>%
  mutate(significativo = ifelse(p.value < 0.05, "Sí", "No"))

write_csv(anova_robusta_df, here("output", "tables", "04_anova_robusta_hc3.csv"))

# Chi - Cuadrado
tabla1 <- table(BlackFriday$Gender, BlackFriday$City_Category)
tabla2 <- table(BlackFriday$Gender, BlackFriday$Age)
tabla3 <- table(BlackFriday$Gender, BlackFriday$Product_Category_1)

chi1 <- chisq.test(tabla1)
chi2 <- chisq.test(tabla2)
chi3 <- chisq.test(tabla3)

chi_df <- tibble(
  comparacion = c("Gender vs City_Category", "Gender vs Age", "Gender vs Product_Category_1"),
  p_value = c(chi1$p.value, chi2$p.value, chi3$p.value)
) %>%
  mutate(
    significativa = ifelse(p_value < 0.05, "Sí", "No")
  )

write_csv(chi_df, here("output", "tables", "04_chi_cuadrado.csv"))

# Residuos del modelo completo
resids <- residuals(modelo_completo)

resid_df <- tibble(
  Min = min(resids),
  Q1  = quantile(resids, 0.25),
  Mediana = median(resids),
  Media = mean(resids),
  Q3 = quantile(resids, 0.75),
  Max = max(resids)
)

write_csv(resid_df, here("output", "tables", "04_residuos.csv"))

# Estadistica por grupo
estadistica_por_grupo <- function(base, grupo){
  base %>% 
    group_by({{ grupo }}) %>%
    summarise(media = mean(Purchase),
              sd    = sd(Purchase),
              n     = n(),
              se    = sd / sqrt(n),
              conf_low = media - qt(0.975, n-1) * se,
              conf_high = media + qt(0.975, n-1) * se,
              .groups = "drop"
    )}

estadistica_por_dos_grupos <- function(base, grupo1, grupo2){
  base %>% 
    group_by({{ grupo1 }}, {{ grupo2 }}) %>%
    summarise(
      media = mean(Purchase),
      sd    = sd(Purchase),
      n     = n(),
      se    = sd / sqrt(n),
      conf_low = media - qt(0.975, n-1) * se,
      conf_high = media + qt(0.975, n-1) * se,
      .groups = "drop"
    )
}

stats_genero <- estadistica_por_grupo(BlackFriday, Gender)
stats_edad <- estadistica_por_grupo(BlackFriday, Age)
stats_ciudad <- estadistica_por_grupo(BlackFriday, City_Category)
stats_product_category_1 <- estadistica_por_grupo(BlackFriday, Product_Category_1)
medias_gender_age <- estadistica_por_dos_grupos(BlackFriday, Gender, Age)

write_csv(stats_genero, here("output", "tables", "04_stats_purchase_genero.csv"))
write_csv(stats_edad, here("output", "tables", "04_stats_purchase_edad.csv"))
write_csv(stats_ciudad, here("output", "tables", "04_stats_purchase_ciudad.csv"))
write_csv(stats_product_category_1, here("output", "tables", "04_stats_purchase_product_category_1.csv"))
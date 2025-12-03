# 05 Gráficos
# Librerias
library(dplyr)      
library(ggplot2)    
library(readr)      
library(here)       
library(scales)     
library(patchwork)  
library(ggtext)
library(stringr)
library(geomtextpath)


# Crear directorios
dir.create(here("output"), showWarnings = FALSE)
dir.create(here("output", "figures"), showWarnings = FALSE)

# Datos
BlackFriday <- readRDS("data/processed/BlackFriday_02.rds")

# Cargar estadísticas 
stats_genero <- read_csv(here("output", "tables", "04_stats_purchase_genero.csv"))
stats_edad <- read_csv(here("output", "tables", "04_stats_purchase_edad.csv"))
stats_ciudad <- read_csv(here("output", "tables", "04_stats_purchase_ciudad.csv"))
stats_product_category_1 <- read_csv(here("output", "tables", "04_stats_purchase_product_category_1.csv"))

# Narrativa
diferencia_genero <- stats_genero %>%
  summarise(
    dif_absoluta = media[Gender == "M"] - media[Gender == "F"],
    dif_pct = round((media[Gender == "M"] / media[Gender == "F"] - 1) * 100, 1)
  )

edad_mayor_gasto <- stats_edad %>% slice_max(media, n = 1)
edad_menor_gasto <- stats_edad %>% slice_min(media, n = 1)
ciudad_mayor <- stats_ciudad %>% slice_max(media, n = 1)
ciudad_menor <- stats_ciudad %>% slice_min(media, n = 1)
producto_estrella <- stats_product_category_1 %>% slice_max(media, n = 1)

# Paleta de colores
color_masculino   <- "#084594"   
color_femenino <- "#f4b6c2"     
color_destacado   <- "#4292c6"   
color_secundario  <- "#c6dbef"   
color_acento      <- "#6baed6"   
color = "#D5A6E8"


# base de graficos
theme_story <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_markdown(face = "bold", size = 14, hjust = 0, 
                                    lineheight = 1.2, margin = margin(b = 5)),
      plot.subtitle = element_markdown(size = 10.5, color = "gray20", hjust = 0, 
                                       lineheight = 1.3, margin = margin(t = 5, b = 15)),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9, color = "gray30"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(15, 15, 15, 15),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Capítulo 1: La brecha de genero

p1 <- ggplot(stats_genero, aes(x = Gender, y = media, fill = Gender)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.25,
    linewidth = 1,
    color = "gray40"
  ) +
  # Flecha de comparación
  annotate("segment", 
           x = 1, xend = 2, 
           y = max(stats_genero$media) + 600, 
           yend = max(stats_genero$media) + 600,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
           color = "black", linewidth = 1.2) +
  annotate("text", 
           x = 1.5, 
           y = max(stats_genero$media) + 900,
           label = paste0("+", diferencia_genero$dif_pct, "%"),
           fontface = "bold", 
           size = 5.5, 
           color = "black") +
  geom_text(
    aes(label = paste0("$", comma(round(media, 0)))),
    vjust = -3,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = c("F" = color_femenino, "M" = color_masculino)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "*Capítulo 1:* La Brecha de Género en Black Friday",
    subtitle = paste0("Los hombres gastan <span style='color:", color_masculino, "'>**", 
                      diferencia_genero$dif_pct, "% más**</span> que las mujeres. ",
                      "Una diferencia de <span style='color:", color_acento, "'>**$", 
                      comma(round(diferencia_genero$dif_absoluta, 0)), "**</span> por compra."),
    x = NULL,
    y = "Gasto Promedio"
  ) +
  theme_story()

# Capítulo 2: Gasto por edad

stats_edad <- stats_edad %>%
  mutate(
    Age = factor(Age, levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+")),
    es_pico = Age == edad_mayor_gasto$Age
  )

p2 <- ggplot(stats_edad, aes(x = Age, y = media, fill = es_pico)) +
  geom_col(alpha = 0.9) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.35,
    linewidth = 0.8,
    color = "gray40"
  ) +
  # Flecha al ganador
  geom_curve(
    data = data.frame(
      x = which(levels(stats_edad$Age) == edad_mayor_gasto$Age) - 0.6,
      xend = which(levels(stats_edad$Age) == edad_mayor_gasto$Age),
      y = edad_mayor_gasto$media + 2600,     # ← SUBIDO
      yend = edad_mayor_gasto$media + 1200   # ← SUBIDO
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    curvature = -0.3,
    color = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  annotate("text", 
           x = which(levels(stats_edad$Age) == edad_mayor_gasto$Age) - 0.9, 
           y = edad_mayor_gasto$media + 2100, 
           label = "PICO\nDE GASTO", 
           fontface = "bold", size = 3.5, 
           color = "grey40", 
           lineheight = 0.85) +
  geom_text(
    aes(label = paste0("$", comma(round(media, 0)))),
    vjust = -1.5,
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = c("FALSE" = color_secundario, "TRUE" = color_destacado)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "*Capítulo 2:* La Madurez Financiera Domina",
    subtitle = paste0("El grupo <span style='color:", color_destacado, "'>**", 
                      edad_mayor_gasto$Age, " años**</span> lidera con <span style='color:", 
                      color_acento, "'>**$", comma(round(edad_mayor_gasto$media, 0)), 
                      "**</span>. ",
                      "Supera en **$", comma(round(edad_mayor_gasto$media - edad_menor_gasto$media, 0)), 
                      "** al grupo más joven."),
    x = "Grupo de Edad",
    y = "Gasto Promedio"
  ) +
  theme_story() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Capítulo 3: Gasto por ciudad

stats_ciudad <- stats_ciudad %>%
  mutate(es_mayor = City_Category == ciudad_mayor$City_Category)

p3 <- ggplot(stats_ciudad, aes(x = reorder(City_Category, -media), y = media, fill = es_mayor)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.25,
    linewidth = 1,
    color = "gray40"
  ) +
  geom_text(
    aes(label = paste0("$", comma(round(media, 0)))),
    vjust = -2.5,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  # Línea promedio
  geom_hline(yintercept = mean(stats_ciudad$media), 
             linetype = "dashed", 
             color = "gray40", 
             linewidth = 0.8) +
  annotate("text",
           x = 2.5,
           y = mean(stats_ciudad$media) + 150,
           label = "← Promedio general",
           size = 3.5,
           color = "black",
           fontface = "italic") +
  scale_fill_manual(values = c("FALSE" = color_secundario, "TRUE" = color_destacado)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "*Capítulo 3:* Ciudad C el Epicentro del Gasto",
    subtitle = paste0("La ciudad <span style='color:", color_destacado, "'>**", 
                      ciudad_mayor$City_Category, "**</span> lidera con <span style='color:", 
                      color_acento, "'>*$", comma(round(ciudad_mayor$media, 0)), "*</span>. ",
                      "Supera en **", round((ciudad_mayor$media/ciudad_menor$media - 1) * 100, 1), 
                      "%** a la ciudad de menor gasto."),
    x = "Categoría de Ciudad",
    y = "Gasto Promedio"
  ) +
  theme_story()

# Capítulo 4: Los productos estrellas

top10_productos <- stats_product_category_1 %>%
  arrange(desc(media)) %>%
  slice_head(n = 10) %>%
  mutate(
    es_top3 = row_number() <= 3,
    categoria_label = paste0("Categoría ", Product_Category_1)
  )

p4 <- ggplot(top10_productos, aes(x = reorder(categoria_label, media), y = media, fill = es_top3)) +
  geom_col(alpha = 0.9) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.35,
    linewidth = 0.7,
    color = "gray40"
  ) +
  geom_text(
    aes(label = paste0("$", comma(round(media, 0)))),
    hjust = -0.15,
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  # Highlight del Top 3
  annotate("rect",
           xmin = 8.5, xmax = 10.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08,
           fill = color_destacado) +
  annotate("text",
           x = 9.5,
           y = max(top10_productos$media) * 0.35,
           label = "TOP 3\nCATEGORÍAS\nPREMIUM",
           fontface = "bold",
           size = 4,
           color = "black",
           lineheight = 0.9) +
  scale_fill_manual(values = c("FALSE" = color_secundario, "TRUE" = color_destacado)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title = "*Capítulo 4:* Las Categorías Premium",
    subtitle = paste0("La categoría <span style='color:", color_destacado, "'>**", 
                      producto_estrella$Product_Category_1, "**</span> lidera con <span style='color:", 
                      color_acento, "'>**$", comma(round(producto_estrella$media, 0)), 
                      "**</span>. ",
                      "El *Top 3* concentra productos de mayor valor agregado."),
    x = NULL,
    y = "Gasto Promedio"
  ) +
  theme_story() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

# Capítulo 5: Intersección de edad y genero

BlackFriday_boxplot <- BlackFriday %>%
  mutate(Age = factor(Age, levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+")))

# Calcular diferencia máxima por edad
dif_por_edad <- BlackFriday_boxplot %>%
  group_by(Age, Gender) %>%
  summarise(media = mean(Purchase), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = media) %>%
  mutate(diferencia_pct = round((M / F - 1) * 100, 1)) %>%
  slice_max(diferencia_pct, n = 1)

p5 <- ggplot(BlackFriday_boxplot, aes(x = Age, y = Purchase, fill = Gender)) +
  geom_boxplot(outlier.alpha = 0.15, outlier.size = 0.5, linewidth = 0.5, alpha = 0.85) +
  # Destacar grupo crítico
  annotate("rect",
           xmin = which(levels(BlackFriday_boxplot$Age) == dif_por_edad$Age) - 0.45,
           xmax = which(levels(BlackFriday_boxplot$Age) == dif_por_edad$Age) + 0.45,
           ymin = -Inf, ymax = Inf,
           alpha = 0.1,
           fill = color_acento) +
  annotate("text",
           x = which(levels(BlackFriday_boxplot$Age) == dif_por_edad$Age),
           y = 23000,
           label = paste0("Brecha máxima:\n+", dif_por_edad$diferencia_pct, "%"),
           fontface = "bold",
           size = 3.2,
           color = "black",  
           lineheight = 0.9) +
  scale_fill_manual(
    values = c("F" = color_femenino, "M" = color_masculino),
    labels = c("F" = "Mujeres", "M" = "Hombres")
  ) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "*Capítulo 5:* La Brecha Se Amplifica con la Edad",
    subtitle = paste0("En el grupo <span style='color:", color_acento, "'>**", 
                      dif_por_edad$Age, " años**</span> la diferencia alcanza su **máximo (+", 
                      dif_por_edad$diferencia_pct, "%)**. ",
                      "Los hombres gastan más en *todos* los grupos etarios."),
    x = "Grupo de Edad",
    y = "Monto de Compra",
    fill = "Género"
  ) +
  theme_story() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.size = unit(0.8, "cm")
  )

# Graficos juntos

combined_plot <- (p1 | p3) / (p2 | p4) / (p5) +
  plot_annotation(
    title = "Black Friday: La Historia Detrás de los Números",
    subtitle = str_wrap(paste0(
      "HALLAZGO PRINCIPAL: El consumo en Black Friday NO es homogéneo. ",
      "Se identifican diferencias significativas por género (", diferencia_genero$dif_pct, "%), ",
      "edad (rango: $", comma(round(edad_menor_gasto$media, 0)), "-$", comma(round(edad_mayor_gasto$media, 0)), "), ",
      "ubicación geográfica y categoría de producto. ",
      "Estos patrones revelan segmentos de mercado claramente diferenciados con comportamientos únicos."
    ), width = 160),
    caption = paste0(
      "Fuente: Black Friday Sales Dataset | n = ", comma(nrow(BlackFriday)), " transacciones | ",
      "Análisis estadístico con intervalos de confianza al 95% | ",
      "*H0 RECHAZADA:* Las variables demográficas SÍ influyen significativamente en el gasto (p < 0.001)"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5, 
                                color = color_destacado, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "gray20", hjust = 0.5, 
                                   lineheight = 1.4, margin = margin(t = 10, b = 25)),
      plot.caption = element_markdown(size = 9, color = "gray40", hjust = 0.5, 
                                      lineheight = 1.3, margin = margin(t = 20)),
      plot.margin = margin(25, 25, 25, 25),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Guardar graficos

# Gráfico combinado principal
ggsave(
  here("output", "figures", "05_historia_black_friday_completa.png"),
  combined_plot,
  width = 18,
  height = 20,
  dpi = 320,
  bg = "white"
)

# Capítulos individuales
ggsave(here("output", "figures", "05_cap1_brecha_genero.png"), 
       p1, width = 9, height = 7, dpi = 320, bg = "white")

ggsave(here("output", "figures", "05_cap2_poder_edad.png"), 
       p2, width = 11, height = 7, dpi = 320, bg = "white")

ggsave(here("output", "figures", "05_cap3_geografia_consumo.png"), 
       p3, width = 9, height = 7, dpi = 320, bg = "white")

ggsave(here("output", "figures", "05_cap4_productos_premium.png"), 
       p4, width = 11, height = 9, dpi = 320, bg = "white")

ggsave(here("output", "figures", "05_cap5_interaccion_edad_genero.png"), 
       p5, width = 13, height = 8, dpi = 320, bg = "white")




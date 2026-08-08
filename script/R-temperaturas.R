# =====================================================================
# VÍA A: ANÁLISIS DE CORRELACIÓN ESPACIO-CLIMÁTICA (R)
# Objetivo: Evaluar el impacto del estrés térmico en la radiancia
# =====================================================================

# 1. Carga de librerías
library(dplyr)
library(ggplot2)
library(stringr)

cat("Ingestando y procesando datos...\n")

# 2. Lectura de datasets
# Asegúrate de que los archivos CSV estén en tu directorio de trabajo
df_viirs <- read.csv("viirs_moderno_mensual.csv")
df_era5 <- read.csv("era5_temperatura_mensual.csv")

# 3. Limpieza y estandarización de vectores temporales y espaciales
df_viirs <- df_viirs %>%
  mutate(
    Fecha = as.Date(Fecha),
    # Limpiamos los nombres de las regiones por si traen etiquetas de GEE
    Region = str_replace(as.character(Region), "_mean", ""),
    Region = str_trim(Region)
  )

df_era5 <- df_era5 %>%
  mutate(
    Fecha = as.Date(Fecha),
    Region = str_trim(as.character(Region))
  )

# 4. Acoplamiento de matrices (Merge Estocástico)
regiones_estudio <- c("Carabobo", "Miranda", "Zulia")

df_acoplado <- inner_join(df_viirs, df_era5, by = c("Fecha", "Region")) %>%
  filter(Region %in% regiones_estudio)

# 5. Cálculo Computacional: Coeficientes de Correlación de Pearson
cat("\nCoeficientes de Correlación de Pearson (Temperatura vs. Radiancia):\n")
correlaciones <- df_acoplado %>%
  group_by(Region) %>%
  summarise(
    r_Pearson = cor(Temperatura_C, Radiancia_VIIRS, use = "complete.obs")
  )
print(correlaciones)

# 6. Generación de Visualización Analítica (Regresión Lineal Simple)
plot_clima <- ggplot(df_acoplado, aes(x = Temperatura_C, y = Radiancia_VIIRS, color = Region)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  facet_wrap(~ Region, scales = "free") +
  theme_minimal() +
  labs(
    title = "Impacto del Estrés Térmico en la Estabilidad de la Red (2014-2026)",
    subtitle = "Correlación entre la Temperatura Media (ERA5) y la Radiancia Servida (VIIRS)",
    x = "Temperatura Media Mensual (°C)",
    y = "Radiancia Física (VIIRS)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12)
  )

# 7. Despliegue y exportación del gráfico
print(plot_clima)
# Descomenta la siguiente línea para guardar la gráfica en alta resolución
# ggsave("temp_vs_rad_R.png", plot = plot_clima, width = 12, height = 5, dpi = 300)

cat("\nAnálisis completado. Matriz evaluada y gráfica generada.\n")
# =====================================================================
# PROYECTO 1: SENSIBILIDAD DEL ANOVA 
# Caso de Estudio: Demanda Eléctrica Regional (Proxy: Radiancia VIIRS)
# =====================================================================

# LIBRERÍAS (Instálalas con install.packages("ggplot2") si no las tienes)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------------------
# FASE 1: INGESTA DE DATOS Y ESTADÍSTICA DESCRIPTIVA (Línea Base)
# ---------------------------------------------------------------------
cat("Cargando datos satelitales...\n")
datos_viirs <- read.csv("viirs_moderno_mensual.csv")

# Filtrar datos de los 3 estados (excluyendo el agregado Nacional)
datos_estados <- subset(datos_viirs, Region %in% c("Carabobo", "Miranda", "Zulia"))

# Calcular la desviación estándar (ruido/inestabilidad) real de cada estado
stats_reales <- datos_estados %>%
  group_by(Region) %>%
  summarise(
    Media = mean(Radiancia_VIIRS),
    Varianza = var(Radiancia_VIIRS),
    Desviacion_Std = sd(Radiancia_VIIRS)
  )

print("Estadísticas Reales Observadas (VIIRS 2014-2026):")
print(stats_reales)

# Tomaremos la varianza más baja observada (suele ser Miranda) como nuestra varianza "base"
# para inyectarla en la simulación.
sd_base <- min(stats_reales$Desviacion_Std)

# ---------------------------------------------------------------------
# FASE 2: SIMULACIÓN DE MONTE CARLO (El requerimiento de la asignación)
# ---------------------------------------------------------------------
# OBJETIVO: Evaluar la Tasa de Error Tipo I bajo Heterocedasticidad y Desbalance.
# REGLA DE ORO DEL ERROR TIPO I: La Hipótesis Nula DEBE ser cierta. 
# Por tanto, forzaremos a que las medias de los 3 grupos sean matemáticamente idénticas,
# y solo manipularemos las varianzas (simulando la inestabilidad eléctrica) y los tamaños de muestra.

num_simulaciones <- 5000
alfa <- 0.05
mu_comun <- 5.0 # Media sintética idéntica para los 3 grupos

# Parámetros de Desbalance Muestral (n diferentes por fallas en sensores/data)
n_miranda  <- 120 # Muestra completa (10 años x 12 meses)
n_carabobo <- 60  # Muestra reducida a la mitad
n_zulia    <- 30  # Desbalance severo (mucha pérdida de datos)

# Ratios de Varianza a evaluar (1x, 2x, 5x, 10x la varianza base)
ratios_var <- c(1, 2, 5, 10)
resultados_error <- numeric(length(ratios_var))

cat("\nIniciando Simulación de Monte Carlo (", num_simulaciones, " iteraciones por ratio)...\n", sep="")

# Bucle sobre los diferentes niveles de heterocedasticidad
for(i in 1:length(ratios_var)) {
  
  rechazos_H0 <- 0
  ratio <- ratios_var[i]
  
  # Asignamos las desviaciones estándar. 
  # A Zulia (la muestra más pequeña) le asignamos la varianza más grande (El peor escenario para ANOVA)
  sd_1 <- sd_base
  sd_2 <- sd_base * sqrt(2)     # Varianza intermedia
  sd_3 <- sd_base * sqrt(ratio) # Varianza multiplicada por el ratio
  
  for(j in 1:num_simulaciones) {
    # 1. Generación de datos sintéticos
    y_miranda  <- rnorm(n_miranda,  mean = mu_comun, sd = sd_1)
    y_carabobo <- rnorm(n_carabobo, mean = mu_comun, sd = sd_2)
    y_zulia    <- rnorm(n_zulia,    mean = mu_comun, sd = sd_3)
    
    # 2. Estructuración
    valores <- c(y_miranda, y_carabobo, y_zulia)
    grupos  <- factor(rep(c("Miranda", "Carabobo", "Zulia"), times = c(n_miranda, n_carabobo, n_zulia)))
    df_sim  <- data.frame(Radiancia = valores, Estado = grupos)
    
    # 3. Prueba ANOVA
    modelo <- aov(Radiancia ~ Estado, data = df_sim)
    p_valor <- summary(modelo)[[1]][["Pr(>F)"]][1]
    
    # 4. Conteo de Error Tipo I (Rechazamos H0 incorrectamente)
    if(p_valor < alfa) {
      rechazos_H0 <- rechazos_H0 + 1
    }
  }
  
  # Registrar la proporción de error para este ratio
  resultados_error[i] <- rechazos_H0 / num_simulaciones
  cat("Ratio de Varianza 1:", ratio, " | Tasa Error Tipo I:", resultados_error[i], "\n")
}

# ---------------------------------------------------------------------
# FASE 3: INFORME TÉCNICO Y GRÁFICAS
# ---------------------------------------------------------------------
df_resultados <- data.frame(Ratio_Varianza = ratios_var, Error_Tipo_I = resultados_error)

grafica <- ggplot(df_resultados, aes(x = Ratio_Varianza, y = Error_Tipo_I)) +
  geom_line(color = "#004B87", linewidth = 1.2) +
  geom_point(color = "#E31837", size = 4) +
  geom_hline(yintercept = alfa, linetype = "dashed", color = "black", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Sensibilidad del ANOVA ante Violación de Supuestos",
    subtitle = "Efecto combinado de Heterocedasticidad y Desbalance Muestral",
    x = "Razón de Varianzas Máxima/Mínima",
    y = "Tasa de Error Tipo I (Falsos Positivos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(grafica)

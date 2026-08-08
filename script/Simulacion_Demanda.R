# =====================================================================
# SCRIPT MAESTRO: CALIBRACIÓN ESTOCÁSTICA DE DEMANDA ELÉCTRICA
# Versión 7.0: Modelo de Inferencia Regional Ponderado (Filtro de Ruido)
# =====================================================================

library(dplyr)
library(stringr)

# 1. INGESTA DE LOS CONJUNTOS DE DATOS REALES
cat("Cargando archivos matrices desde el directorio de trabajo...\n")
df_dmsp  <- read.csv("dmsp_historico_anual.csv")
df_viirs <- read.csv("viirs_moderno_mensual.csv")

# 2. SANEAMIENTO DE ESTRUCTURAS
df_dmsp_limpio <- df_dmsp %>% 
  mutate(Anio = as.numeric(as.character(Anio)))

df_viirs_limpio <- df_viirs %>% 
  mutate(
    Anio = as.numeric(format(as.Date(Fecha), "%Y")),
    Region = str_replace(as.character(Region), "_mean", "")
  )

# 3. FILTRADO EXCLUSIVO DE REGIONES SOCIO-INDUSTRIALES (Anti-Flaring Gas)
# Excluimos el agregador "Nacional" para limpiar el ruido de quema de pozos petroleros
regiones_estudio <- c("Carabobo", "Miranda", "Zulia")

df_dmsp_reg <- df_dmsp_limpio %>% filter(Region %in% regiones_estudio)
df_viirs_reg <- df_viirs_limpio %>% filter(Region %in% regiones_estudio)

# 4. CALIBRACIÓN DE LÍNEA BASE HISTÓRICA (Año 2000)
# En el año 2000, la carga combinada estimada de Carabobo, Miranda y Zulia 
# representaba aproximadamente el 45% de la demanda nacional máxima (5,535 MW de los 12,300 MW)
demanda_regiones_2000 <- 12300 * 0.45 

luz_regiones_2000 <- df_dmsp_reg %>% 
  filter(Anio == 2000) %>% 
  summarise(Total = sum(Intensidad_DMSP_0_a_63)) %>% 
  pull(Total)

luz_regiones_2013 <- df_dmsp_reg %>% 
  filter(Anio == 2013) %>% 
  summarise(Total = sum(Intensidad_DMSP_0_a_63)) %>% 
  pull(Total)

# Factor de contracción en nodos de consumo real
factor_contraccion_reg <- luz_regiones_2013 / luz_regiones_2000
demanda_regiones_equivalente_2013 <- demanda_regiones_2000 * factor_contraccion_reg

# 5. ACOPLAMIENTO CON LA ERA MODERNA REGIONAL (2014)
luz_viirs_reg_2014 <- df_viirs_reg %>%
  filter(Anio == 2014) %>%
  group_by(Anio, Region) %>%
  summarise(Media_Mensual = mean(Radiancia_VIIRS), .groups = "drop") %>%
  summarise(Total_Radiancia = sum(Media_Mensual)) %>%
  pull(Total_Radiancia)

# Coeficiente Kappa enfocado en centros de carga activos
kappa_regional <- luz_viirs_reg_2014 / demanda_regiones_equivalente_2013
cat("Coeficiente Kappa Regionalizado (Libre de Ruido):", kappa_regional, "\n")

# 6. ESTIMACIÓN DE LA DEMANDA ACTUAL EN MEGAVATIOS (Expansión de la Base de Datos)
# Para dar validez y robustez matemática, integramos los últimos 36 meses completos 
# de la serie (2021-2023) para evaluar la estabilidad de la demanda remanente.
df_actual_bloque <- df_viirs_reg %>%
  filter(Anio %in% c(2021, 2022, 2023)) %>%
  group_by(Fecha) %>%
  summarise(Radiancia_Combinada = sum(Radiancia_VIIRS), .groups = "drop")

cat("Puntos muestrales mensuales validados para la inferencia robusta:", nrow(df_actual_bloque), "\n")

# Escalamiento de la carga regional a la equivalencia Nacional (Factor de ponderación 1 / 0.45)
demanda_nacional_proyectada <- (df_actual_bloque$Radiancia_Combinada / kappa_regional) / 0.45

# Inferencia estadística final mediante t-test al 99% de confianza
prediccion_demanda <- t.test(demanda_nacional_proyectada, conf.level = 0.99)

demanda_estimada_media <- prediccion_demanda$estimate
limite_inferior_99     <- prediccion_demanda$conf.int[1]
limite_superior_99     <- prediccion_demanda$conf.int[2]

# 7. EMISIÓN DEL DICTAMEN FINAL ROBUSTO
cat("\n=====================================================================\n")
cat("RESULTADOS DEL MODELO INFERENCIAL REGIONALIZADO V7.0\n")
cat("=====================================================================\n")
cat("Demanda Media Nacional Actual Estimada:    ", round(demanda_estimada_media, 2), "MW\n")
cat("Intervalo de Confianza Estocástico al 99%: [", round(limite_inferior_99, 2), " , ", round(limite_superior_99, 2), "] MW\n")
cat("Umbral del Discurso Oficial Evaluado:      ", 14000, "MW\n")

if(limite_superior_99 < 14000) {
  cat("\nCONCLUSIÓN ESTADÍSTICA: Se RECHAZA la hipótesis oficial de los 14,000 MW.\n")
  cat("Al remover los sesgos geográficos, la demanda real se ubica significativamente por debajo.\n")
} else {
  cat("\nCONCLUSIÓN ESTADÍSTICA: NO se puede rechazar el umbral oficial.\n")
  cat("Los intervalos contienen el valor de 14,000 MW bajo la variabilidad regional observada.\n")
}
cat("=====================================================================\n")
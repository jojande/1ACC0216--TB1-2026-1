rm(list=ls(all=TRUE)) 
graphics.off() 
cat("\014") 

install.packages("lattice",dependencies= TRUE)
install.packages("plotly",dependencies= TRUE)
install.packages("lubridate",dependencies = TRUE)
library(lattice)
library(plotly)
library(ggplot2)
library(patchwork)
library(lubridate)
library(dplyr)
Sys.setlocale("LC_TIME", "C")
#---------------------------CARGAR DATOS--------------------------------------------
#-----------------------------------------------------------------------------------
setwd("C:/Users/nohel/OneDrive/Documentos/UPC/2026-01/DataScience/1ACC0216--TB1-2026-1/data")
df <- read.csv('hotel_bookings.csv', header= TRUE, sep=',',dec='.') 
#Para poder ver los tipo de datos por columna
str(df) 
#Para ver las 6 primeras filas del dataset
head(df)

#-------------------------INSPECCIONAR DATOS---------------------------------------
#----------------------------------------------------------------------------------

#Variables categoricas a factores
columnas_categoricas <- c("hotel", "meal", "country", "market_segment", 
                          "distribution_channel", "reserved_room_type", 
                          "assigned_room_type", "deposit_type", 
                          "customer_type", "reservation_status")

df[columnas_categoricas] <- lapply(df[columnas_categoricas], as.factor)

# variables bool a factor
df$is_canceled <- as.factor(df$is_canceled)
df$is_repeated_guest <- as.factor(df$is_repeated_guest)

# texto a date
df$reservation_status_date <- as.Date(df$reservation_status_date)

# nuevas columnas

# Une el año, mes y día en una sola cadena y luego la convierte a formato fecha
df$arrival_date_full <- as.Date(paste(df$arrival_date_year, 
                                      df$arrival_date_month, 
                                      df$arrival_date_day_of_month, sep = "-"), 
                                format = "%Y-%B-%d")

# tiempo total de estancia
df$total_stay_nights <- df$stays_in_weekend_nights + df$stays_in_week_nights

# total de huespedes
# Se usa ifelse para manejar los posibles valores nulos (NA) en 'children'
df$total_guests <- df$adults + ifelse(is.na(df$children), 0, df$children) + df$babies

# bool de si tiene hijos
df$has_kids <- ifelse((ifelse(is.na(df$children), 0, df$children) + df$babies) > 0, "Yes", "No")
df$has_kids <- as.factor(df$has_kids)
#-----------------------------------------------------------------------------------------
#---------------------------------PRE_PROCESAR  DATOS-------------------------------------
#-----------------------------------------------------------------------------------------
##########################################################################################
#-------------------------Resumir Estadísticas Básicas:-----------------------------------

summary(df)

# verificar dimension
dim(df)
#Tipos de datos por columna
tipos_columnas <- data.frame(
  Variable = names(df),
  Tipo_Dato = sapply(df, class)
)
tipos_columnas
#------------------------------------------------------------------------------------------
#-------------------------------IDENTIFICACIÓN DE DATOS FALTANTES--------------------------
# FIJAR CUANTOS VALORES SON N/A ---
na_report <- colSums(is.na(df))
print("--- REPORTE DE VALORES N/A DETECTADOS ---")
print(na_report[na_report > 0])
#------------------------------------------------------------------------------------------
#--------------------------------TRATAMIENTO DE DATOS FALTANTES----------------------------

#Imputación de variables numéricas a la variable Children(Mediana)
df$children[is.na(df$children)] <- median(df$children, na.rm = TRUE)

#Imputación de variables categóricas (Moda)
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Aplicado a la variable country
df$country[is.na(df$country)] <- get_mode(df$country)

# Tratamiento de Agent y Company (Valores desconocidos)
df$agent[is.na(df$agent)] <- 0
df$company[is.na(df$company)] <- 0
adr_original <- df$adr

# Método de Tukey para definir límites
Q1 <- quantile(df$adr, 0.25)
Q3 <- quantile(df$adr, 0.75)
IQR <- Q3 - Q1

lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR




# Técnica de Winsorización: Ajustar valores fuera de los límites
df$adr[df$adr > lim_sup] <- lim_sup
df$adr[df$adr < lim_inf] <- lim_inf

# Aseguramos que no haya precios negativos
df$adr[df$adr < 0] <- 0

#7. REPRESENTACIÓN GRÁFICA PARA EL ANÁLISIS
par(mfrow = c(1, 2))
boxplot(adr_original, 
        main = "ADR con Anomalías", 
        col = "tomato", 
        horizontal = TRUE)

boxplot(df$adr, 
        main = "ADR tras Winsorización", 
        col = "lightgreen", 
        horizontal = TRUE)

par(mfrow = c(1, 1))

#------------------------------------------------------------------------------------------
#--------------------------------DETECTAR OUTLIERS-----------------------------------------
#------------------------------------------------------------------------------------------
#LEAD  TIME
library(ggplot2)
p2<-ggplot(df, aes(x =df$lead_time)) + 
  geom_boxplot(fill="steelblue") + 
  labs(title = "Anomalias de Lead Time", 
  )+ 
  theme_classic() 
p2 
outliers<-boxplot(df$lead_time,plot=FALSE)$out
outliers
# Calcular el IQR para la variable 'df$Lead_Time' 
Q1 <- quantile(df$lead_time, 0.25) 
Q3 <- quantile(df$lead_time, 0.75) 
IQR <- Q3 - Q1 
# Limites inferior y superior 
lower_bound <- Q1 - 1.5 * IQR 
upper_bound <- Q3 + 1.5 * IQR 
# Identificar datos atípicos  de df$Lead Time
outliers <- df$lead_time[df$lead_time < lower_bound | df$lead_time > upper_bound
] 
outliers
#se cambio que si pasan los 365 dias de reserva , esta pase a ser 365 (Esto para que el 
#promedio se mantenga) y no halla valores atipicos
df <- df %>%
  mutate(lead_time = ifelse(lead_time > 365, 365, lead_time))

############################################################################################
#----------------------stays_in_weekend_nights(Noches de fin de semana----------------------
#############################################################################################
#Se decidio reemplazar eliminar  las cuales  filas  que tienen por ejemplo más de 5 noches de 
# fin de semana ,porque se considera como maximo  
p_weekend_antes <- ggplot(df, aes(x = stays_in_weekend_nights)) + 
  geom_boxplot(fill = "steelblue", alpha = 0.7) + 
  labs(title = "Stays Weekend: Antes",
       subtitle = "Con valores > 5 noches") + 
  theme_classic()

valores_outliers_weekend <- boxplot(df$stays_in_weekend_nights, plot = FALSE)$out
print(paste("Se encontraron", length(valores_outliers_weekend), "registros considerados atípicos."))
df <- df %>%
  filter(stays_in_weekend_nights <= 5)

p_weekend_despues <- ggplot(df, aes(x = stays_in_weekend_nights)) + 
  geom_boxplot(fill = "lightgreen", alpha = 0.7) + 
  labs(title = "Stays Weekend: Después",
       subtitle = "Filtrado (Máximo 5 noches)") + 
  theme_classic()

#  MOSTRAR AMBAS GRÁFICAS 
p_weekend_antes + p_weekend_despues

#----------------------stays_in_week_nights (Noches de semana)----------------------
p_week_antes <- ggplot(df, aes(x = stays_in_week_nights)) + 
  geom_boxplot(fill = "steelblue", alpha = 0.7) + 
  labs(title = "Noches Semana: Antes",
       subtitle = "Datos con valores extremos") + 
  theme_classic()

#  IDENTIFICAR OUTLIERS (Opcional, para reporte)
valores_outliers_week <- boxplot(df$stays_in_week_nights, plot = FALSE)$out
print(paste("Se detectaron", length(valores_outliers_week), "atípicos en noches de semana."))

#  APLICAR FILTRADO (Criterio: Máximo 10 noches de semana)
# Este filtro elimina el ruido de estancias exageradamente largas
df <- df %>%
  filter(stays_in_week_nights <= 10)

# GUARDAR LA GRÁFICA "DESPUÉS"
p_week_despues <- ggplot(df, aes(x = stays_in_week_nights)) + 
  geom_boxplot(fill = "lightgreen", alpha = 0.7) + 
  labs(title = "Noches Semana: Después",
       subtitle = "Filtrado (Máximo 10 noches)") + 
  theme_classic()

#  MOSTRAR AMBAS GRÁFICAS JUNTAS
p_week_antes + p_week_despues


#--------------------------ADULTOS POR RESERVA-------------------------------------


# Usamos factor(adults) para que el eje X sea discreto (1, 2, 3...)
p_adults_antes <- ggplot(df, aes(x = factor(adults))) + 
  geom_bar(fill = "steelblue", alpha = 0.7) + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size = 3) +
  labs(title = "Adultos: Antes",
       subtitle = "Distribución original con ruido",
       x = "Número de Adultos",
       y = "Cantidad de Reservas") + 
  theme_classic()


# Eliminamos registros con 0 adultos (no válidos) y más de 6 (ruido/atípicos)
df <- df %>%
  filter(adults > 0 & adults <= 6)

#GUARDAR LA GRÁFICA "DESPUÉS"
p_adults_despues <- ggplot(df, aes(x = factor(adults))) + 
  geom_bar(fill = "lightgreen", alpha = 0.7) + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size = 3) +
  labs(title = "Adultos: Después",
       subtitle = "Filtrado: 1 a 6 adultos",
       x = "Número de Adultos",
       y = "Cantidad de Reservas") + 
  theme_classic()

# MOSTRAR AMBAS GRÁFICAS JUNTAS
p_adults_antes + p_adults_despues


#------------------days_in_waiting_list (Días en lista de espera)------------------

#  GUARDAR LA GRÁFICA "ANTES"
# Usamos escala logarítmica para poder ver algo más que la barra del cero
p_waiting_antes <- ggplot(df, aes(x = days_in_waiting_list)) + 
  geom_histogram(fill = "steelblue", color = "white", bins = 50) + 
  scale_y_log10() + 
  labs(title = "Lista de Espera: Antes",
       subtitle = "Escala Log10 - Con valores >= 365 días",
       x = "Días", y = "Reservas (Log10)") + 
  theme_classic()

# Eliminamos registros que superan un año de espera (365 días)
df <- df %>%
  filter(days_in_waiting_list < 365)

# 3. GUARDAR LA GRÁFICA "DESPUÉS"
p_waiting_despues <- ggplot(df, aes(x = days_in_waiting_list)) + 
  geom_histogram(fill = "lightgreen", color = "white", bins = 50) + 
  scale_y_log10() + 
  labs(title = "Lista de Espera: Después",
       subtitle = "Escala Log10 - Filtrado < 365 días",
       x = "Días", y = "Reservas (Log10)") + 
  theme_classic()

# 4. MOSTRAR AMBAS GRÁFICAS JUNTAS
p_waiting_antes + p_waiting_despues

# Gráfico de Temporadas
library(ggplot2)

df_efectivas <- df %>% filter(is_canceled == 0)

temporadas <- df_efectivas %>%
  group_by(arrival_date_month) %>%
  summarise(total_reservas = n()) %>%
  arrange(desc(total_reservas))

print("Tabla de Temporadas:")
print(temporadas)

ggplot(temporadas, aes(x = reorder(arrival_date_month, total_reservas), y = total_reservas)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Reservas por Mes (Temporadas)", x = "Mes", y = "Total de Reservas") +
  theme_minimal()


#ANALIZAR ESTANCIA PROMEDIO

estancia_promedio <- df_efectivas %>%
  group_by(hotel) %>%
  summarise(promedio_noches = mean(total_stay_nights, na.rm = TRUE))

#Mostrar tabla de estancias en consola
print("Tabla de Estancia Promedio:")
print(estancia_promedio)

#Gráfico de Estancias
grafico_estancias <- ggplot(estancia_promedio, aes(x = hotel, y = promedio_noches, fill = hotel)) +
  geom_col() +
  geom_text(aes(label = round(promedio_noches, 2)), vjust = -0.5) +
  labs(title = "Duración Promedio de Estancia por Hotel", x = "Tipo de Hotel", y = "Noches Promedio") +
  theme_minimal()

print(grafico_estancias)  


# Gráfico de Estancia Promedio
ggplot(estancia_promedio, aes(x = hotel, y = promedio_noches, fill = hotel)) +
  geom_col() +
  labs(title = "Duración Promedio de Estancia", x = "Tipo de Hotel", y = "Noches Promedio") +
  theme_minimal()


# ==============================================================================
# PREGUNTA 7: ¿En qué meses del año se producen más cancelaciones?
# ==============================================================================

# 1. Filtrar solo las reservas canceladas
df_cancelaciones <- df[df$is_canceled == "1" | df$is_canceled == "Canceled", ]

# 2. Crear el gráfico de barras de cancelaciones por mes
# Nota: Usamos el dataframe 'df' completo para comparar o 'df_cancelaciones' para totales
p_cancelaciones_mes <- ggplot(df_cancelaciones, aes(x = arrival_date_month, fill = hotel)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size=3.5) +
  labs(title = "Total de Cancelaciones por Mes",
       subtitle = "Análisis de frecuencia mensual segmentado por tipo de hotel",
       x = "Mes de Llegada",
       y = "Número de Cancelaciones",
       fill = "Tipo de Hotel") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas para lectura

# Mostrar el gráfico
print(p_cancelaciones_mes)

# 3. Resumen tabular para el informe
tabla_cancelaciones <- table(df_cancelaciones$arrival_date_month)
print("Resumen de cancelaciones por mes:")
print(tabla_cancelaciones)

# =============================================================================
# PREGUNTA 1
# ¿Cuántas reservas no canceladas hay por tipo de hotel?
# =============================================================================

reservas_validas <- df %>%
  filter(is_canceled == 0)

reservas_por_hotel <- reservas_validas %>%
  group_by(hotel) %>%
  summarise(total_reservas = n())

print(reservas_por_hotel)

ggplot(reservas_por_hotel,
       aes(x = hotel,
           y = total_reservas,
           fill = hotel)) +
  
  geom_bar(stat = "identity") +
  
  geom_text(aes(label = total_reservas),
            vjust = -0.3,
            size = 5) +
  
  labs(
    title = "Reservas no canceladas por tipo de hotel",
    x = "Tipo de hotel",
    y = "Cantidad de reservas"
  ) +
  
  theme_minimal()

# =============================================================================
# PREGUNTA 2
# ¿Está aumentando la demanda con el tiempo?
# =============================================================================

demanda_tiempo <- reservas_validas %>%
  group_by(arrival_date_full) %>%
  summarise(total_reservas = n())

ggplot(demanda_tiempo,
       aes(x = arrival_date_full,
           y = total_reservas)) +
  
  geom_line(color = "blue", linewidth = 1) +
  
  labs(
    title = "Demanda de reservas a lo largo del tiempo",
    x = "Fecha",
    y = "Cantidad de reservas"
  ) +
  
  theme_minimal()

# -------------------------------------------------------------------------
# PREGUNTA 5: ¿Cuántas reservas incluyen niños y/o bebés?
# -------------------------------------------------------------------------


kids_summary <- df %>%
  group_by(has_kids) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

p1 <- ggplot(kids_summary, aes(x = has_kids, y = n, fill = has_kids)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No" = "#E69F00", "Yes" = "#56B4E9")) +
  labs(title = "Reservas con Niños o Bebés",
       x = "¿Tiene niños/bebés?",
       y = "Cantidad de Reservas",
       fill = "Presencia de niños") +
  theme_minimal()
p1

# -------------------------------------------------------------------------
# PREGUNTA 6: ¿Es importante contar con espacios de estacionamiento?
# -------------------------------------------------------------------------

df$parking_factor <- ifelse(df$required_car_parking_spaces > 0, "Requiere", "No Requiere")

parking_summary <- df %>%
  group_by(parking_factor) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

p2 <- ggplot(parking_summary, aes(x = parking_factor, y = n, fill = parking_factor)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No Requiere" = "#999999", "Requiere" = "#009E73")) +
  labs(title = "Importancia del Estacionamiento",
       subtitle = "Basado en la demanda del cliente",
       x = "Solicitud de Estacionamiento",
       y = "Cantidad de Reservas",
       fill = "Estado") +
  theme_minimal()


p2

# ==============================================================================
# PREGUNTA #8: DISTRIBUCIÓN DE PRECIOS POR TIPO DE HOTEL
# ==============================================================================

# 1. Crear el gráfico de densidad para comparar ADR
p_densidad_adr <- ggplot(df, aes(x = adr, fill = hotel)) +
  geom_density(alpha = 0.5) + # Alpha para ver la transparencia donde se cruzan
  scale_fill_manual(values = c("City Hotel" = "#2c7fb8", "Resort Hotel" = "#7fcdbb")) +
  labs(title = "Distribución de la Tarifa Diaria (ADR) por Tipo de Hotel",
       subtitle = "Comparativa de la concentración de precios tras la limpieza de outliers",
       x = "Tarifa Diaria Promedio (ADR)",
       y = "Densidad (Frecuencia Relativa)",
       fill = "Tipo de Hotel") +
  theme_minimal()

# Mostrar el gráfico
print(p_densidad_adr)

# 2. Resumen estadístico para apoyar el gráfico
resumen_precios <- df %>%
  group_by(hotel) %>%
  summarise(Precio_Medio = mean(adr),
            Precio_Mediano = median(adr))
print(resumen_precios


#-------------------------------------------------------------------------------------------
#-------------------------- GUARDAR EL DATASET FINAL LIMPIO --------------------------------
#-------------------------------------------------------------------------------------------


nombre_archivo_final <- "hotel_bookings_cleaned.csv"


write.csv(df, nombre_archivo_final, row.names = FALSE)


cat("\n========================================================\n",
    "¡PROCESO DE LIMPIEZA COMPLETADO CON ÉXITO!\n",
    "Archivo guardado como:", nombre_archivo_final, "\n",
    "Dimensiones finales:", dim(df)[1], "filas y", dim(df)[2], "columnas.\n",
    "========================================================\n")

#-------------------------------------------------------------------------------------------
#-------------------------- RESUMEN DE LA DATA LIMPIA --------------------------------------
#-------------------------------------------------------------------------------------------

# Ver un pequeño resumen de cómo quedaron las variables clave después de tus filtros
summary(df[c("lead_time", "adr", "stays_in_weekend_nights", 
             "stays_in_week_nights", "adults", "days_in_waiting_list")])



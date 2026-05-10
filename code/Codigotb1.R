rm(list=ls(all=TRUE)) 
graphics.off() 
cat("\014") 
setwd("C:/Users/nohel/Documents/UPC/2026-01/DataScience")
df <- read.csv('hotel_bookings.csv', header= TRUE, sep=',',dec='.') 
#Para poder ver los tipo de datos por columna
str(df) 
#Para ver las 6 primeras filas del dataset
head(df)

#PRE-PROCESAR DATOS
#Resumir Estadísticas Básicas:
summary(df)
#Identificar de datos Faltantes:
colSums(is.na(df))
#Tratamiento de Datos Faltantes:
#Usamos la imputación porque son pocos datos
df$children[is.na(df$children)] <-0
#----Para verificar que no hay N/A
sum(is.na(df$children))

#Persona 1
#Detectar filas duplicadas 
duplicados <- sum(duplicated(df))
duplicados
# Eliminar duplicados
df <- df[!duplicated(df), ]
# verificar nueva dimension
dim(df)

#Tipos de datos por columna
tipos_columnas <- data.frame(
  Variable = names(df),
  Tipo_Dato = sapply(df, class)
)
tipos_columnas



#Cambios de Tipo de Datos

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

#Guardar data set limpio
write.csv(
  df,
  "hotel_bookings_limpio.csv",
  row.names = FALSE
)



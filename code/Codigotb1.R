rm(list=ls(all=TRUE)) 
graphics.off() 
cat("\014") 
setwd("C:/Users/User/Desktop/Dataset_TB1_CD")
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
#Guardar data set limpio
write.csv(
  df,
  "hotel_bookings_limpio.csv",
  row.names = FALSE
)


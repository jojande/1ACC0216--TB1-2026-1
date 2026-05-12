# 1ACC0216--TB1-2026-1

## Objetivo del trabajo
* Realizar un análisis exploratorio de un conjunto de datos (EDA) para encontrar patrones de comportamiento.
* Generar visualizaciones, preparar los datos y extraer conclusiones iniciales utilizando R/RStudio como herramienta de software.

## Nombre de los alumnos participantes
* Jose Andres Diaz Orihuela
* Antony Rodrigo Quito Anccasi
* Lieserl Noemi Ayala Condori 
* Mateo Alonso Monge Jiménez

## Breve descripción del dataset
* El conjunto de datos analizado se denomina "Hotel booking demand".
* Contiene información de las reservas de un hotel urbano ("City Hotel") y un hotel de tipo resort ("Resort Hotel").
* Originalmente cuenta con 119,390 observaciones y 32 variables, e incluye datos sobre fechas de reserva, duración de la estadía, cantidad de huéspedes (adultos, niños, bebés) y requerimientos de estacionamiento.
* Para este análisis académico, el dataset fue modificado incorporando ruido, lo que implica la presencia de datos faltantes (NA) y datos atípicos (outliers) que fueron tratados en la fase de pre-procesamiento.

## Conclusiones
1. **Preferencia de Hotel:** El City Hotel tiene una demanda significativamente mayor, concentrando el 61.5% de las reservas efectivas (46,228) en comparación con el Resort Hotel (28,938).
2. **Estacionalidad:** Existe una fuerte estacionalidad con un pico máximo en los meses de verano (agosto con 8,592 y julio con 7,865 reservas), mientras que enero, diciembre y noviembre representan la temporada baja con menor ocupación.
3. **Duración de la Estancia:** La duración de estadía refleja el modelo de negocio: el Resort Hotel tiene un promedio más alto (4.05 noches) orientado al ocio, mientras que el City Hotel tiene una mayor rotación (2.91 noches), lo cual es característico del sector corporativo y viajes cortos.
4. **Composición de Huéspedes:** El mercado principal está compuesto por adultos sin menores a cargo. El 92.14% (69,255) de las reservas efectivas no incluyeron niños ni bebés.
5. **Estacionamiento:** La disponibilidad de espacios de estacionamiento no es un factor crítico; cerca del 92% de los clientes no requieren este servicio.
6. **Cancelaciones:** Aunque agosto presenta la mayor cantidad absoluta de cancelaciones (5,239), enero muestra la mayor tasa de cancelaciones en términos porcentuales (43.4%), lo que indica mayor incertidumbre en las reservas de temporada baja.

## Licencia
Este proyecto se encuentra bajo la Licencia MIT. Consulta el archivo `LICENSE` en el repositorio para más detalles.
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(readr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(VIM)
library(mlr)
library(lubridate)
library(stringr)
library(readxl)

# LECTURA DE DATOS

ventas <- read_excel("GitHub/Willy-AI/dataset/ReporteVentas.xlsx",skip=7)
colnames(ventas) <- c("numero", "documento", "fecha", "importe_total", "orden_compra",
                      "codigo", "descripcion", "categoria", "precio_unitario",
                      "cantidad", "total_item")
summary(ventas)
str(ventas)

ventas$documento <- as.factor(ventas$documento)
ventas$codigo <- as.factor(ventas$codigo)
ventas$descripcion <- as.factor(ventas$descripcion)
ventas$categoria <- as.factor(ventas$categoria)
ventas$fecha <- as.POSIXct(ventas$fecha, tryFormats = c("%d/%m/%Y %H:%M", "%d/%m/%Y"))
ventas$precio_unitario <- as.numeric(ventas$precio_unitario)
ventas$cantidad <- as.numeric(ventas$cantidad)
ventas$total_item <- as.numeric(ventas$total_item)

summary(ventas)
str(ventas)

#Deteccion y limpieza de valores nulos

aggr(ventas, numbers=T, sortVar=T)
ventas <- ventas[!is.na(ventas$documento), ]

ventas.limpia <- impute(ventas,
                        classes = list(factor=imputeMode(),
                                       integer = imputeMode(),
                                       numeric = imputeMedian()),
                        dummy.classes = c("integer","factor"),
                        dummy.type = "numeric")
ventas.limpia <- ventas.limpia$data[,1:min(dim(ventas))]
summary(ventas.limpia)
aggr(ventas.limpia, numbers=T, sortVar=T)

#Deteccion de outliners
ggplot(ventas.limpia,aes(x=precio_unitario))+
  geom_boxplot(fill="steelblue")+
  labs(title = "Distribución de precios unitarios", 
    x = "Precio unitario S/")+
  theme_classic()

Q1 <- quantile(ventas.limpia$precio_unitario, 0.25, na.rm = TRUE)
Q3 <- quantile(ventas.limpia$precio_unitario, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lim_superior <- Q3 + 1.5 * IQR_val

cat("\nPrecios unitarios considerados altos:\n")
outliers_precio <- ventas.limpia %>% 
  filter(precio_unitario > lim_superior) %>%
  arrange(desc(precio_unitario)) %>%
  select(descripcion, categoria, precio_unitario)
print(head(outliers_precio, 10))

ggplot(ventas.limpia, aes(y = precio_unitario)) +
  geom_boxplot(fill="steelblue", outlier.color="red", outlier.shape=16) +
  labs(title = "Distribución de precios unitarios",
       y = "Precio unitario (S/)") +
  theme_classic()


ventas.limpia <- ventas.limpia[ventas.limpia$cantidad > 0 & ventas.limpia$total_item > 0, ]
ventas.limpia <- ventas.limpia[!is.na(ventas.limpia$fecha), ]

# Resultados

#Ventas totales por mes
ventas_mensuales <- ventas.limpia %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(ventas_totales = sum(total_item, na.rm = TRUE))

ggplot(ventas_mensuales, aes(x = mes, y = ventas_totales)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Ventas totales por mes",
       x = "Mes", y = "Total de ventas (S/)") +
  theme_minimal()

#Unidades mas vendidas en Julio 2025
ventas_julio <- ventas.limpia %>%
  filter(month(fecha) == 7) 

top_julio_cantidades <- ventas_julio %>%
  group_by(descripcion) %>%
  summarise(cantidad_total = sum(cantidad, na.rm = TRUE)) %>%
  arrange(desc(cantidad_total)) %>%
  slice(1:10)

print(top_julio_cantidades)

ggplot(top_julio_cantidades, aes(x = reorder(descripcion, cantidad_total), y = cantidad_total)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 productos con mayor cantidad vendida en Julio",
       x = "Producto",
       y = "Cantidad vendida (unidades)") +
  theme_minimal()

#Ventas semanales
ventas_semana <- ventas.limpia %>%
  mutate(dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  group_by(dia_semana) %>%
  summarise(ventas_promedio = mean(total_item, na.rm = TRUE),
            cantidad_promedio = mean(cantidad, na.rm = TRUE))

ggplot(ventas_semana, aes(x = dia_semana, y = ventas_promedio)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Promedio de ventas por día de la semana",
       x = "Día", y = "Ventas promedio (S/)") +
  theme_minimal()

#10 productos más vendidos
top_productos_cant <- ventas.limpia %>%
  group_by(descripcion) %>%
  summarise(cantidad_total = sum(as.numeric(cantidad), na.rm = TRUE)) %>%
  arrange(desc(cantidad_total)) %>%
  slice(1:10)

ggplot(top_productos_cant, aes(x = reorder(descripcion, cantidad_total), y = cantidad_total)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", cantidad_total)), 
            hjust = -0.1, color = "black", size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 productos más vendidos por cantidad",
       x = "Producto",
       y = "Cantidad total vendida (unidades)") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=0.5))

#Promedio diario de ventas por mes
ventas_categoria <- ventas.limpia %>%
  group_by(categoria) %>%
  summarise(cantidad_total = sum(as.numeric(cantidad), na.rm = TRUE)) %>%
  arrange(desc(cantidad_total))

ventas_mensual_promedio <- ventas.limpia %>%mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%summarise(promedio_diario = mean(total_item, na.rm = TRUE))

ggplot(ventas_mensual_promedio, aes(x = mes, y = promedio_diario)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Promedio diario de ventas por mes",
       x = "Mes", y = "Promedio de ventas (S/)") +
  theme_minimal()


#Evolucion mensual de ventas por articulo (10 mejores)
top_articulos <- ventas.limpia %>%
  group_by(descripcion) %>%
  summarise(total_cantidad = sum(as.numeric(cantidad), na.rm = TRUE)) %>%
  arrange(desc(total_cantidad)) %>%
  slice(1:10) %>%
  pull(descripcion)

ventas_articulo_mes <- ventas.limpia %>%
  filter(descripcion %in% top_articulos) %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes, descripcion) %>%
  summarise(cantidad_total = sum(as.numeric(cantidad), na.rm = TRUE), .groups = "drop")

ggplot(ventas_articulo_mes, aes(x = mes, y = cantidad_total, color = descripcion, group = descripcion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Evolución mensual de ventas por artículo (Top 10)",
       x = "Mes",
       y = "Cantidad vendida (unidades)",
       color = "Producto") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))

#Relacion entre precio y cantidad vendida
ventas_filtradas <- ventas.limpia %>%
  filter(precio_unitario <= 50) 

ggplot(ventas_filtradas, aes(x = precio_unitario, y = cantidad)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre precio y cantidad vendida (precios ≤ S/ 50)",
       x = "Precio unitario (S/)",
       y = "Cantidad vendida (unidades)") +
  theme_minimal()

#Evolucion de precios de los 3 productos mas vendidos
productos_seleccionados <- c("GRANEL: Arroz Faraón", "GRANEL: Huevos", "GRANEL: Azucar Rubia")

precio_varios <- ventas.limpia %>%
  filter(descripcion %in% productos_seleccionados, precio_unitario <= 10) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha, descripcion) %>%
  summarise(precio_promedio = mean(precio_unitario, na.rm = TRUE), .groups = "drop")

ggplot(precio_varios, aes(x = fecha, y = precio_promedio, color = descripcion, group = descripcion)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolución comparativa de precios por producto (≤ S/ 10)",
       x = "Fecha",
       y = "Precio promedio (S/)",
       color = "Producto") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

#Guardado del dataset limpio

write_csv(ventas.limpia, "ventas_limpias.csv", na = "")

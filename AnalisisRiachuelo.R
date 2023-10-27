library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(paletteer)
paleta = paletteer_d("ggsci::category20b_d3")

data_mediciones_geolocaliazadas_dummies <- read_csv("/Users/agusvaser/Documents/Personal/riachuelo-clieaning/data_mediciones_geolocaliazadas_dummies.csv")

data_mediciones_geolocaliazadas_dummies %>% summary()
View(data_mediciones_geolocaliazadas_dummies)

colnames(data_mediciones_geolocaliazadas_dummies)

data_mediciones_geolocaliazadas_dummies$Fecha %>% summary()

nrow(data_mediciones_geolocaliazadas_dummies)

data1 = data_mediciones_geolocaliazadas_dummies %>% group_by(Estación, Fecha) %>% mutate(nivel = case_when(
  `Cumple Limites Ia`== TRUE ~ 1,
  `Cumple Limites Ib`== TRUE ~ 2,
  `Cumple Limites II`== TRUE ~ 3,
  `Cumple Limites III`== TRUE ~ 4,
  `Cumple Limites IV`== TRUE ~ 5,
  `Cumple Limites IV`== FALSE ~ 0,
  T ~ NaN
))

dfsinna = na.omit(data_mediciones_geolocaliazadas_dummies)
dfsinna = dfsinna %>% mutate(nivel = case_when(
  `Cumple Limites Ia`== TRUE ~ 5,
  `Cumple Limites Ib`== TRUE ~ 4,
  `Cumple Limites II`== TRUE ~ 3,
  `Cumple Limites III`== TRUE ~ 2,
  `Cumple Limites IV`== TRUE ~ 1,
  `Cumple Limites IV`== FALSE ~ 0,
))

# Tabla de frecuencia de niveles
table(dfsinna$nivel)
View(dfsinna)

tabla_resumen <- aggregate(dfsinna$nivel, by = list(dfsinna$Estación, dfsinna$Fecha,dfsinna$Cuenca,dfsinna$Subcuenca), FUN = min)
colnames(tabla_resumen) <- c("Estacion", "Fecha","Cuenca","Sub Cuenca", "Nivel_Minimo")

registros_por_fecha_estacion <- aggregate(nivel ~ Cuenca + Subcuenca + Fecha + Estación, data = dfsinna, FUN = length)
colnames(registros_por_fecha_estacion) <- c("Cuenca","Sub Cuenca", "Fecha", "Estacion", "Registros")


# Agrupado por estacion, fecha, otorgandole un nivel minimo adaptando a la peor medida y 
# la cantidad de medidad observadas
resumen_filtrado <- merge(tabla_resumen, registros_por_fecha_estacion)
resumen_filtrado <- resumen_filtrado[resumen_filtrado$Registros >= 9, ]
resumen_filtrado[resumen_filtrado$Nivel_Minimo==5,]

# Cantidad de niveles por cuenca
tabla_resumen %>%  summarise(n = n(), .by = c(Cuenca, Nivel_Minimo))

# Grafico 1
# Evolucion del nivel agrupado año a año de cada cuenca
promedios <- dfsinna %>%
  group_by(Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(nivel))

grafico <- ggplot(promedios, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Cuenca, color = Cuenca)) +
  geom_line() +
  labs(x = "Año", y = "Promedio de Nivel", title = "Evolución Anual del Promedio de Nivel por Cuenca")

grafico

# Grafico 1 desde nivel cumple iv

# promedios <- dfsinna %>%
#   group_by(Cuenca, year(Fecha)) %>%
#   summarize(Promedio_Nivel = mean(`Cumple Limites IV`))
# 
# grafico <- ggplot(promedios, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Cuenca, color = Cuenca)) +
#   geom_line() +
#   labs(x = "Año", y = "Promedio de Nivel", title = "Evolución Anual del Promedio de Nivel por Cuenca")
# 
# grafico

# Grafico 2
# Evolucion del nivel agrupado año a año de cada subcuenca de su cuenca respectiva
promedios2 <- dfsinna %>%
  group_by(Subcuenca,Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(nivel))

grafico2 <- ggplot(promedios2, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Subcuenca, color = factor(Subcuenca))) +
  geom_line() +
  labs(x = "Año", y = "Promedio de Nivel", title = "Evolución Anual del Promedio de Nivel por Cuenca", color = "Subcuenca") + facet_wrap(~Cuenca) + scale_color_manual(values = paleta) 

grafico2


# Grafico 3
# Cantidad de registros por subcuenca dentro de sus respectivas cuencas
resumen <- dfsinna %>%
  group_by(Cuenca, Subcuenca) %>%
  summarize(n = n()) %>%
  ungroup()

# Crear el gráfico de barras apiladas
grafico3 <- ggplot(resumen, aes(x = Cuenca, y = n, fill = factor(Subcuenca))) +
  geom_bar(stat = "identity") +
  labs(x = "Cuenca", y = "Cantidad de Registros", title = "Cantidad de registros por subcuenca dentro de cada cuenca", fill = "Subcuenca") + scale_fill_manual(values = paleta)

grafico3

# Evolucion de registros a lo largo de los años

cantidad = dfsinna %>% group_by(year(Fecha)) %>% summarize(n = n())
graficoq <- ggplot(cantidad, aes(x = `year(Fecha)`, y = n)) +
  geom_line() +
  labs(x = "Año", y = "Cantidad de registros")


# Tabla de frecuencia parametros
tabla_proporcion <- dfsinna %>%
  mutate(año = year(Fecha)) %>%
  group_by(año, Medida) %>%
  summarize(Proporcion_TRUE = mean(cbind(`Cumple Limites Ia`,`Cumple Limites Ib`,`Cumple Limites II`,`Cumple Limites III`,`Cumple Limites IV`)))


# Tabla de varianzas por parametro
varianzas = tabla_proporcion %>% 
  group_by(Medida) %>% 
  summarize(Varianza = var(Proporcion_TRUE)) %>% 
  arrange(desc(Varianza))

top5vars = varianzas %>% head(5)

# Evolucion de las variables que mas fueron variando

tabla_proporcion_variables = tabla_proporcion %>% filter(Medida %in% top5vars$Medida)

grafico4 <- ggplot(tabla_proporcion_variables, aes(x = año, y = Proporcion_TRUE, group = Medida, color = Medida)) +
  geom_line() +
  labs(x = "Año", y = "Promedio de Nivel", title = "Evolución  de los 5 parametros que mayor variaron", color = "Subcuenca") 

grafico4

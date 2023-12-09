library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(paletteer)
paleta = paletteer_d("ggsci::category20b_d3")

data_mediciones_geolocaliazadas_dummies <- read_csv("data_mediciones_geolocaliazadas_dummies.csv")
data_nueva <- read_csv("data_mediciones_geolocaliazadas_dummies.csv")

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

dfsinna_2 = na.omit(data_nueva) %>% filter(Tipo == "Agua Superficial")
dfsinna_2 = dfsinna_2 %>% mutate(nivel = case_when(
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

promediosiv <- tabla_resumen %>%
  group_by(Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(Nivel_Minimo))

dfsinna %>%
  group_by(Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(`Cumple Limites IV`))

graficoiv <- ggplot(promediosiv, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Cuenca, color = Cuenca)) +
  geom_line() +
  labs(x = "Año", y = "Promedio de nivel de agua")

graficoiv

# Grafico 1
# Evolucion del nivel agrupado año a año de cada cuenca
promedios <- dfsinna %>%
  group_by(Cuenca, year(Fecha)) %>%
  filter(`year(Fecha)` == 2021 | `year(Fecha)` == 2022) %>%
  summarize(Promedio_Nivel = mean(nivel))

grafico <- ggplot(promedios, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Cuenca, color = Cuenca)) +
  geom_line() +
  labs(x = "Año", y = "Promedio de Nivel", title = "Evolución Anual del Promedio de Nivel por Cuenca")

grafico

# Grafico 1 desde nivel cumple iv

promediosiv <- dfsinna %>%
  group_by(Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(`Cumple Limites III`), n = n()) %>%
  arrange(desc(`year(Fecha)`))

graficoiv <- ggplot(promediosiv, aes(x = `year(Fecha)`, y = Promedio_Nivel, group = Cuenca, color = Cuenca)) +
  geom_line() +
  labs(x = "Año", y = "Proporcion de aprobación de cumplimiento para la categoria IV")

graficoiv

# Grafico 2
# Evolucion del nivel agrupado año a año de cada subcuenca de su cuenca respectiva
promedios2 <- tabla_resumen %>%
  group_by(`Sub Cuenca`,Cuenca, year(Fecha)) %>%
  summarize(Promedio_Nivel = mean(Nivel_Minimo))

grafico2 <- ggplot(promedios2 %>% filter(`Sub Cuenca` == 0), aes(x = `year(Fecha)`, y = Promedio_Nivel, group = `Sub Cuenca`, color = factor(`Sub Cuenca`))) +
  geom_line() +
  labs(x = "Año", y = "Promedio de Nivel") + facet_wrap(~Cuenca) + scale_color_manual(values = paleta)

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
  labs(x = "Cuenca", y = "Cantidad de Registros", fill = "Subcuenca") + scale_fill_manual(values = paleta)

grafico3

# Evolucion de registros a lo largo de los años

cantidad = dfsinna %>% group_by(year(Fecha)) %>% summarize(n = n())
graficoq <- ggplot(cantidad, aes(x = `year(Fecha)`, y = n)) +
  geom_line() + labs(x = "Año", y = "Cantidad de registros")

graficoq

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

dfsinna %>% group_by(Estación)
dfsinna %>% group_by(Estación, Fecha) %>% summarise(F2 = sum(`Cumple Limites IV` == FALSE))
dfsinna %>% group_by(Estación, Fecha) %>% summarise(F2 = sum(`Cumple Limites IV` == FALSE) / n())
dfsinna %>% group_by(Estación, Fecha) %>% summarise(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())
dfsinna %>% group_by(Estación, Fecha) %>% summarise(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())


dfsinna %>% group_by(Estación, year(Fecha)) %>% summarise(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())


dfsinna_2 %>% group_by(Estación, year(Fecha)) %>% summarise(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())

dfsinna_2 %>%
  mutate(Di = ifelse(`Cumple Limites IV` == FALSE & Tipo_Limite == ">", `Valor (=)` / Limite, Limite / `Valor (=)`)) %>%
  group_by(Estación, year(Fecha)) %>%
  mutate(F3 = (sum(Di) / n()) / (0.01 * (sum(Di) / n()) + 0.01))


DF_3 <- dfsinna_2 %>%
  group_by(Estación, year(Fecha), Medida) %>%
  summarise(n = n(), mean =  mean(`Valor (=)`), sup = case_when(
    Tipo_Limite == ">" ~ mean(`Valor (=)`) > mean(as.numeric(`Limite IV`)),
    Tipo_Limite == "<" ~ mean(`Valor (=)`) < mean(as.numeric(`Limite IV`)),
    T ~ mean(`Valor (=)`) < 9 && mean(`Valor (=)`) > 6,
  ))


DF_4 <- DF_3[!duplicated(DF_3), ] %>% group_by(Estación, `year(Fecha)`) %>% mutate(F1 = sum(sup) / n())



  #sup = mean(`Valor (=)`) / mean(as.numeric(`Limite IV`))) %>% mutate(F1 = 100 * sum(`Cumple Limites IV` == FALSE) / n())
#df_2 <- df_2 %>% group_by(Estación, year(Fecha)) %>% mutate(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())

# F1
DF_4 <- data %>%
  group_by(Estación, year(Fecha)) %>%
  summarise(failed_params = sum(any(`Cumple Limites IV` == FALSE)), total_params = n_distinct(Medida)) %>%
  mutate(F1 = failed_params / total_params * 100)

# F2
DF_5 <- dfsinna_2 %>%
  group_by(Estación, year(Fecha)) %>%
  summarise(F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n())

# F3
DF_6 <- dfsinna_2 %>%
  mutate(Di = case_when(
    `Cumple Limites IV` == TRUE ~ 0,
    Tipo_Limite == ">" ~ `Valor (=)` / as.numeric(`Limite IV`),
    Tipo_Limite == "<" ~ as.numeric(`Limite IV`) / `Valor (=)`,
    `Valor (=)` > as.numeric(substr(`Limite IV`, nchar(`Limite IV`), nchar(`Limite IV`))) ~ `Valor (=)` / as.numeric(substr(`Limite IV`, nchar(`Limite IV`), nchar(`Limite IV`))),
    `Valor (=)` < as.numeric(substr(`Limite IV`, 1, 1)) ~ as.numeric(substr(`Limite IV`, 1, 1)) / `Valor (=)`,
    T ~ 0
  )) %>%
  group_by(Estación, year(Fecha)) %>%
  summarise(F3 = (sum(Di) / n()) / (0.01 * (sum(Di) / n()) + 0.01))

# Merge all variables into dfsinna_2
dfsinna_2 <- dfsinna_2 %>% mutate('year(Fecha)' = year(Fecha))
dfsinna_2 <- left_join(dfsinna_2, DF_4, by = c("Estación", "year(Fecha)"))
dfsinna_2 <- left_join(dfsinna_2, DF_5, by = c("Estación", "year(Fecha)"))
dfsinna_2 <- left_join(dfsinna_2, DF_6, by = c("Estación", "year(Fecha)"))

DF <- dfsinna_2 %>% select(2,7,20, 21, 26, 27, 28)

DF <- DF %>% mutate(ICA = 100 - (sqrt(F1^2 + F2^2 + F3^2) / 1.732))

promedios_DF <- DF %>%
  group_by(Cuenca, year(Fecha)) %>%
  summarize(Promedio_ICA = mean(ICA), n = n()) %>%
  arrange(desc(`year(Fecha)`))

graficoiv <- ggplot(promedios_DF, aes(x = `year(Fecha)`, y = Promedio_ICA, group = Cuenca, color = Cuenca)) +
  geom_line() +
  labs(x = "Año", y = "Proporcion de aprobación de cumplimiento para la categoria IV")

graficoiv


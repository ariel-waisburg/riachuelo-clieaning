library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(paletteer)
library(shiny)
library(VennDiagram)
library(treemap)

paleta = paletteer_d("ggsci::category20b_d3")

data_original <- read_csv("data_mediciones_geolocaliazadas_dummies.csv")
data = na.omit(data_original) %>% filter(Tipo == "Agua Superficial")
data$Cuenca = substr(data$Cuenca, 8, nchar(data$Cuenca))


# Indice de Calidad de Agua en relacion al cumplimiento del Uso IV --------

# F1 y F2
data_f12 <- data %>%
  group_by(Estación, year(Fecha)) %>%
  summarise(
    failed_params = sum(any(`Cumple Limites IV` == FALSE)),
    total_params = n_distinct(Medida),
    F2 = 100 * sum(`Cumple Limites IV` == FALSE) / n()) %>%
  mutate(F1 = 100 * failed_params / total_params)

# F3
data_f3 <- data %>%
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

# Merge all variables into data
data_ICA <- data %>%
  mutate('year(Fecha)' = year(Fecha)) %>%
  group_by(Cuenca, Subcuenca, Estación, year(Fecha)) %>%
  summarise(n = n())
data_ICA <- left_join(data_ICA, data_f12, by = c("Estación", "year(Fecha)"))
data_ICA <- left_join(data_ICA, data_f3, by = c("Estación", "year(Fecha)"))
data_ICA <- data_ICA %>% mutate(ICA = 100 - (sqrt(F1^2 + F2^2 + F3^2) / 1.732))

# Analisis ICA por Cuenca por Ano
data_ICA_cuenca <- data_ICA %>%
  group_by(Cuenca, `year(Fecha)`) %>%
  summarize(Promedio_ICA = mean(ICA), n = n()) %>%
  arrange(desc(`year(Fecha)`))

ggplot(data_ICA_cuenca, aes(x = `year(Fecha)`, y = Promedio_ICA, group = Cuenca, color = Cuenca)) +
  geom_line() +
  geom_ribbon(aes(ymin = 60, ymax = 65), fill = "orange", alpha = 0.05) +
  geom_text(aes(x = 2015, y = 62, label = "Mala"), color = "grey", size = 3, alpha = 0.2) +
  geom_ribbon(aes(ymin = 65, ymax = 80), fill = "yellow", alpha = 0.05) +
  geom_text(aes(x = 2015, y = 75, label = "Regular"), color = "grey", size = 3, alpha = 0.2) +
  geom_ribbon(aes(ymin = 80, ymax = 95), fill = "lightgreen", alpha = 0.05) +
  geom_text(aes(x = 2015, y = 87, label = "Buena"), color = "grey", size = 3, alpha = 0.2) +
  geom_ribbon(aes(ymin = 95, ymax = 100), fill = "green", alpha = 0.05) +
  geom_text(aes(x = 2015, y = 97, label = "Excelente"), color = "grey", size = 3, alpha = 0.2) +
  scale_x_continuous(breaks = seq(2008, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(60, 100, by = 2.5)) +
  labs(x = "Año", y = "Promedio del ICA en relacion al cumplimiento del Uso IV") +
  theme(legend.position = "bottom", legend.margin = margin(t = 10), axis.text.y = element_text(margin = margin(r = 10)))

data_ICA_cuenca_var <- data_ICA_cuenca %>%
  arrange(Cuenca, `year(Fecha)`) %>%
  mutate(Interannual_Variation_ICA = Promedio_ICA - lag(Promedio_ICA))

ggplot(data_ICA_cuenca_var, aes(x = `year(Fecha)`, y = Interannual_Variation_ICA, group = Cuenca, color = Cuenca)) +
  geom_line() +
  scale_x_continuous(limits = c(2009, 2023), breaks = seq(2009, 2023, by = 1)) +
  scale_y_continuous(limits = c(-12.5, 12.5), breaks = seq(-12.5, 12.5, by = 2.5), labels = scales::percent_format(scale = 1)) +
  labs(x = "Año", y = "Variación interanual del ICA en relacion al cumplimiento del Uso IV") +
  theme(legend.position = "bottom", legend.margin = margin(t = 10), axis.text.y = element_text(margin = margin(r = 10)))

# Analisis ICA por Cuenca
data_ICA_cuenca_sin_tiempo <- data_ICA %>%
  group_by(Cuenca) %>%
  summarize(Promedio_ICA = mean(ICA), n = n())

ggplot(data_ICA_cuenca_sin_tiempo, aes(x = reorder(Cuenca, -Promedio_ICA), y = Promedio_ICA, fill = "lightblue3")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio ICA por Cuenca",
       y = "Promedio ICA",
       x = "Cuenca") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(values = "lightblue3") +  # Set fill color to lightblue3
  theme_minimal() +
  guides(fill = FALSE)  # Remove the fill legend

# Analisis ICA por Subcuenca por Ano (Interactivo con Shiny)
data_ICA_subcuenca <- data_ICA %>%
  group_by(Cuenca,Subcuenca, `year(Fecha)`) %>%
  summarize(Promedio_ICA = mean(ICA), n = n()) %>%
  arrange(Subcuenca)

ui <- fluidPage(
  titlePanel("Interactive Plot of ICA by Subcuenca"),
  sidebarLayout(
    sidebarPanel(
      selectInput("subcuenca", "Select Subcuenca:", choices = unique(data_ICA_subcuenca$Subcuenca), selected = NULL)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filter(data_ICA_subcuenca, Subcuenca == input$subcuenca)
  })

  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = `year(Fecha)`, y = Promedio_ICA, color = Subcuenca)) +
      geom_line() +
      labs(x = "Año", y = "ICA")
  })
}

shinyApp(ui, server)

# Analisis ICA por Estacion
data_ICA_estacion_sin_tiempo <- data_ICA %>%
  group_by(Estación) %>%
  summarize(Promedio_ICA = mean(ICA), n = n())

# Analisis ICA por Estacion Actualizado
data_ICA_estacion_actualizado <- data_ICA %>%
  arrange(desc(`year(Fecha)`)) %>%
  head(102)
data_ICA_estacion_actualizado <- left_join(data_ICA_estacion_actualizado, data[,2:4], by = c("Estación"), multiple = "first")

write.csv(data_ICA_estacion_actualizado, "~/Code/Work/riachuelo_cleaning/riachuelo-clieaning/data_ICA_actualizado.csv", row.names = FALSE)

# Treemap
sub_nest <- data_ICA[!duplicated(data_ICA$Estación),2:3] %>% group_by(Subcuenca) %>% summarise(sub_est = n())
df_treemap <- data %>%
             group_by(Cuenca, Subcuenca) %>%
             summarize(n = n_distinct(Fecha))
df_treemap <- left_join(df_treemap, sub_nest, by = "Subcuenca")
df_treemap$n_ratio <- df_treemap$n / df_treemap$sub_est
df_treemap <- data.frame(df_treemap)
treemap(df_treemap,
        index=c("Cuenca","Subcuenca"),
        vSize="n_ratio",
        type="index",
        palette = c("ALTA" = "#E41A1C", "BAJA" = "#00FF00", "MEDIA" = "#0000FF"),
        title="",
        fontsize.title=12,
)

data_estaciones <- data_ICA %>%
  group_by(Estación) %>%
  summarize(Promedio_ICA = mean(ICA), n = as.numeric(n() * n)) %>% ungroup()

ggplot(data_estaciones, aes(x = Promedio_ICA, y = n)) +
  geom_point() +
  labs(title = "Scatter Plot of Promedio_ICA vs. n",
       x = "Promedio_ICA",
       y = "n")

# Parametros interactivo

data_parametros <- data %>%
  group_by(Medida, year(Fecha)) %>%
  summarise(n = n())
colnames(data_parametros)[1] <- "Parametro"

ggplot(data_parametros, aes(x = `year(Fecha)`, y = n, color = Parametro, group = Parametro)) +
  geom_line() +
  scale_x_continuous(limits = c(2008, 2023), breaks = seq(2008, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 1700), breaks = seq(0, 1700, by = 100)) +
  labs(x = "Año", y = "Cantidad de medidas") +
  theme(legend.position = "bottom", legend.margin = margin(t = 10), axis.text.y = element_text(margin = margin(r = 10)))

ui <- fluidPage(
  titlePanel("Cantidad de Medidas por Parametro"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Parametro", "Select Parametro:", choices = unique(data_parametros$Parametro), selected = NULL)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filter(data_parametros, Parametro == input$Parametro)
  })

  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = `year(Fecha)`, y = n, color = Parametro)) +
      geom_line() +
      scale_x_continuous(limits = c(2008, 2023), breaks = seq(2008, 2023, by = 1)) +
      scale_y_continuous(limits = c(0, 1700), breaks = seq(0, 1700, by = 100)) +
      labs(x = "Año", y = "Cantidad de medidas") +
      theme(legend.position = "bottom", legend.margin = margin(t = 10), axis.text.y = element_text(margin = margin(r = 10)))
  })
}

shinyApp(ui, server)

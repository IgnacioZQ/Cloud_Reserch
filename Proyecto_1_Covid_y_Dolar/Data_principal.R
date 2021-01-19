# Librerias ----

library(RCurl)
library(readxl)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(gifski)
library(readr)

# Cargar Data ----

Covid <- read.csv("https://raw.githubusercontent.com/IgnacioZQ/Cloud_Reserch/main/Proyecto_1_Covid_y_Dolar/covid.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

Dolar = read_excel("Dolar.xls")

# Manipular Data

Covid <- select(Covid, Fecha, Casos.totales)
Covid$Fecha <- as.Date(as.character(Covid$Fecha))

{Covid = select(Covid, Fecha, Casos.totales) %>%
  filter(Fecha <="2020-12-30")
Dolar = select(Dolar, Periodo, Dólar) %>%
  filter(Periodo >"2020-03-01")}

Covid <- Covid %>%
  mutate(Fecha = ymd(Fecha))

Dolar <- Dolar %>%
  mutate(Fecha = ymd(Periodo)) %>%
  select(-Periodo, Fecha, Dólar)

# Unir Fechas en una sola Data (New_DF)

New_DF <- full_join(Covid, Dolar, by = "Fecha")

######## Gráfico solo Covid.

ggplot(data = Covid, aes(x = Fecha, y = Casos.totales)) +
  geom_point(colour = "red") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000), labels = scales::label_number()) +
  geom_smooth(method = 'lm',formula = 'y ~ x', se = FALSE, color = "black")

######## Gráfico solo Dolar.

ggplot(data = Dolar, aes(x = Fecha, y = Dólar)) +
geom_point(colour = "red") +
  ggtitle("N° de casos de Coronavirus año 2020 Chile") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(550, 600, 650, 700, 750, 800, 850, 900)) +
  geom_smooth(method = 'lm',formula='y ~ x', se = FALSE, color = "black") +
  ylab("Valor en CLP")

######## Gráfico ambos juntos.




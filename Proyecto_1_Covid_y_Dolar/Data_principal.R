# Librerias ----

{library(RCurl)
library(readxl)
library(tidyverse)
library(ggrepel)}

# Cargar Data ----

dolar= read_excel("Dolar.xls")

Covid <- select(Covid, Fecha, Casos.totales)
Covid$Fecha <- as.Date(as.character(Covid$Fecha))

Covid= select(Covid, Fecha) %>%
  filter(Fecha <="2020-12-30")
Dolar= select(Dolar, Periodo) %>%
  filter(Periodo >"2020-03-01")

########

ggplot(data = Covid, aes(x = Fecha, y = Casos.totales)) +
  geom_point(colour = "red") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000), labels = scales::label_number()) +
  geom_smooth(method = 'lm',formula = 'y ~ x', se = FALSE, color = "black")

########

ggplot(data = Dolar, aes(x = Periodo, y = Dólar)) +
geom_point(colour = "red") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(550, 600, 650, 700, 750, 800, 850, 900)) +
  geom_smooth(method = 'lm',formula='y ~ x', se = FALSE, color="black")

######## Juntar data

data_1 <- merge(Dolar, Covid)


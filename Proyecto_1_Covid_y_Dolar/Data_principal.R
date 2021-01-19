# Librerias ----

library(RCurl)
library(readxl)
library(tidyverse)
library(ggrepel)
library(lubridate)
install.packages("gifski")
library(gifski)

# Cargar Data ----

Dolar = read_excel("Dolar.xls")

Covid <- getURL("https://raw.githubusercontent.com/IgnacioZQ/Cloud_Reserch/main/Proyecto_1_Covid_y_Dolar/covid.csv")
  Covid <- read.csv(text = Covid)

Covid <- select(Covid, Fecha, Casos.totales)
Covid$Fecha <- as.Date(as.character(Covid$Fecha))

Covid= select(Covid, Fecha) %>%
  filter(Fecha <="2020-12-30")
Dolar= select(Dolar, Periodo) %>%
  filter(Periodo >"2020-03-01")

Covid <- Covid %>%
  mutate(Fecha = ymd(Fecha)) %>%
  select(-d)

Dolar <- Dolar %>%
  mutate(Fecha = ymd(Periodo)) %>%
  select(-Periodo, Fecha, Dólar)

# Unir Fechas

New_Dt <- full_join(Covid, Dolar, by = "Fecha")

# LUBRIDATE

mean(is.na(New_Dt$Dólar))

########

ggplot(data = Covid, aes(x = Fecha, y = Casos.totales)) +
  geom_point(colour = "red") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000), labels = scales::label_number()) +
  geom_smooth(method = 'lm',formula = 'y ~ x', se = FALSE, color = "black")

########

ggplot(data = Dolar, aes(x = Fecha, y = Dólar)) +
geom_point(colour = "red") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(550, 600, 650, 700, 750, 800, 850, 900)) +
  geom_smooth(method = 'lm',formula='y ~ x', se = FALSE, color = "black") +
  transition_reveal(Fecha) +
  ylab("Valor en CLP")

######## Juntar data

data_1 <- merge(Dolar, Covid)



# Librerias ----

{library(RCurl)
library(readxl)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(gifski)
library(readr)
library(hrbrthemes)
library(countrycode)}

# Cargar Data ----

{Covid <- read.csv("https://raw.githubusercontent.com/IgnacioZQ/Cloud_Reserch/main/Proyecto_1_Covid_y_Dolar/covid.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

Dolar = read_excel("Dolar.xls")}

# Manipular Data

Covid <- select(Covid, Fecha, Casos.totales)
Covid$Fecha <- as.Date(as.character(Covid$Fecha))

{Covid = select(Covid, Fecha, Casos.totales) %>%
  filter(Fecha <="2020-12-30")
Dolar = select(Dolar, Periodo, D�lar) %>%
  filter(Periodo >"2020-03-01")}

Covid <- Covid %>%
  mutate(Fecha = ymd(Fecha))

Dolar <- Dolar %>%
  mutate(Fecha = ymd(Periodo)) %>%
  select(-Periodo, Fecha, D�lar)

# Unir Fechas en una sola Data (New_DF)

New_DF <- full_join(Covid, Dolar, by = "Fecha")

######## Gr�fico solo Covid (Connie)

ggplot(data = Covid, aes(x = Fecha, y = Casos.totales)) +
  geom_point(colour = "red") +
  ggtitle("Diagrama de dispersi�n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000), labels = scales::label_number()) +
  geom_smooth(method = 'lm',formula = 'y ~ x', se = FALSE, color = "black")

######## Gr�fico solo Dolar (Connie)

ggplot(data = Dolar, aes(x = Fecha, y = D�lar)) +
geom_point(colour = "red") +
  ggtitle("N� de casos de Coronavirus a�o 2020 Chile") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(550, 600, 650, 700, 750, 800, 850, 900)) +
  geom_smooth(method = 'lm',formula='y ~ x', se = FALSE, color = "black") +
  ylab("Valor en CLP")

######## Gr�fico solo Covid (Nacho)

New_DF %>%
  ggplot(aes(x = Fecha, y = Casos.totales)) +
  geom_area(size = 1, fill = "gold", color = "gold", alpha = 0.4) +
  labs(title = paste0(""), 
       subtitle = paste0("") , 
       caption = "", 
       y = "", 
       x = "") +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 600000), labels = scales::label_number()) +
  theme_minimal()

######## Gr�fico solo Dolar (Nacho)

New_DF_na_omit <- na.omit(New_DF) # Eliminar valores vacios en la DF
New_DF_na_omit$Fecha <- as.Date(as.character(New_DF_na_omit$Fecha))

New_DF_na_omit %>%
  ggplot(aes(x = Fecha, y = D�lar)) +
  geom_line(color = "gold", alpha = 0.8, size = 1) +
  labs(title = paste0(""), 
       subtitle = paste0("") , 
       caption = "", 
       y = "", 
       x = "") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(750, 800, 850)) +
  theme_minimal() +
  geom_smooth(se = F, color = "sandybrown", size = 0.8)


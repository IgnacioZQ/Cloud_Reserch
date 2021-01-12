# Librerias ----

{library(RCurl)
library(readxl)
library(tidyverse)}

# Cargar Data de Covid ----

{Covid <- getURL("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv")
Covid <- read.csv(text = Covid)}

# Cargar Data de Dolar ----

Dolar <- read_excel("Dolar.xls")

# Análisis exploratorio ----

# Dolar

summary(Dolar$Dólar)

plot(Dolar)

# Covid

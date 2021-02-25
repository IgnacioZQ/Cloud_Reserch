# Librerias ----

{library(tidyverse)
library(readr)}

# Cargar Data (Como WHR_2020) ----

WHR_2020 <- read_csv("World_Happiness_Report_2020.csv")

#### Percepcion de corrupcion ----

Data <- select(WHR_2020, "Country name", "Regional indicator", "Perceptions of corruption")
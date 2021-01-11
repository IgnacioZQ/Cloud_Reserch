library(RCurl)
library(readxl)

{Covid <- getURL("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv")
Covid <- read.csv(text = Covid, row.names = 1)
Covid <- as.data.frame(t(Covid))} # Invertir columnas y filas, ya que el csv venia invertido.

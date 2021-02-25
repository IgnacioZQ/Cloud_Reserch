# Librerias ----

{library(tidyverse)
library(readr)}

# Theme ----

theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = NA)) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

# Cargar Data (Como WHR_2020) ----

WHR_2020 <- read_csv("World_Happiness_Report_2020.csv")

#### Indice de libertad de vida ----

# Manipular Data ----

WHR_2020_2 <- WHR_2020 %>%
  filter(WHR_2020$`Freedom to make life choices` >= 0.74)

# Cambio de nombre a español (Paises)
{ WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Azerbaijan"] = "Azerbaiyán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Bahrain"] = "Bahréin"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Belgium"] = "Bélgica"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Brazil"] = "Brasil"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Cambodia"] = "Camboya"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Cameroon"] = "Camerún"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Canada"] = "Canadá"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Cyprus"] = "Chipre"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Czech Republic"] = "Republica Checa"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Denmark"] = "Dinamarca"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Dominican Republic"] = "República Dominicana"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Ethiopia"] = "Etiopía"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Finland"] = "Finlandia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "France"] = "Francia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Germany"] = "Alemania"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Hong Kong S.A.R. of China"] = "RAE de Hong Kong. de China"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Iceland"] = "Islandia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Ireland"] = "Irlanda"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Japan"] = "Japón"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Jordan"] = "Jordán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Kazakhstan"] = "Kazajstán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Kenya"] = "Kenia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Kyrgyzstan"] = "Kirguistán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Libya"] = "Libia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Lithuania"] = "Lituania"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Luxembourg"] = "Luxemburgo"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Malaysia"] = "Malasia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Maldives"] = "Maldivas"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Mauritius"] = "Mauricio"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Mexico"] = "México"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Morocco"] = "Marruecos"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Netherlands"] = "Países Bajos"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "New Zealand"] = "Nueva Zelanda"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Niger"] = "Níger"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "North Cyprus"] = "Chipre del norte"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Norway"] = "Noruega"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Panama"] = "Panamá"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Peru"] = "Perú"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Philippines"] = "Filipinas"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Poland"] = "Polonia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Romania"] = "Rumania"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Rwanda"] = "Ruanda"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Saudi Arabia"] = "Arabia Saudita"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Singapore"] = "Singapur"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Slovakia"] = "Eslovaquia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Slovenia"] = "Eslovenia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "South Africa"] = "Sudáfrica"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Spain"] = "España"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Sweden"] = "Suecia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Switzerland"] = "Suiza"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Taiwan Province of China"] = "Taiwan, provincia de China"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Tajikistan"] = "Tayikistán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Thailand"] = "Tailandia"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Trinidad and Tobago"] = "Trinidad y Tobago"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "Turkmenistan"] = "Turkmenistán"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "United Arab Emirates"] = "Emiratos Árabes Unidos"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "United Kingdom"] = "Reino Unido"
WHR_2020_2$`Country name`[WHR_2020_2$`Country name` == "United States"] = "Estados Unidos"}

# Cambio de nombre a español (Continentes) ----
{ WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Central and Eastern Europe"] = "Europa Central y del Este"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Commonwealth of Independent States"] = "Estados Independientes"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "East Asia"] = "Este de Asia"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Latin America and Caribbean"] = "América Latina y el Caribe"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Middle East and North Africa"] = "Oriente Medio y Africa del Norte"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "North America and ANZ"] = "América del Norte y ANZ"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "South Asia"] = "Asia del Sur"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Southeast Asia"] = "Sudeste de Asia"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Sub-Saharan Africa"] = "Africa Sub-Sahariana"
WHR_2020_2$`Regional indicator`[WHR_2020_2$`Regional indicator` == "Western Europe"] = "Europa Oriental"}

Data_1.1 <- WHR_2020_2 %>%
  select("Country name", "Regional indicator", "Freedom to make life choices") %>%
  filter(WHR_2020_2$`Freedom to make life choices` >= 0.859)

Data_1.2 <- WHR_2020_2 %>%
  select("Country name", "Regional indicator", "Freedom to make life choices") %>%
  filter(WHR_2020_2$`Freedom to make life choices` < 0.859)

# Graficar ----
# Exportar en 1080 x 650

# Parte 1
Data_1.1 %>%
  arrange(`Freedom to make life choices`) %>%
  mutate(`Country name` = factor(`Country name`, levels = `Country name`)) %>%
  ggplot(aes(x = `Country name`, y = `Freedom to make life choices`)) +
  geom_segment(aes(x = `Country name`, xend = `Country name`, y = `Freedom to make life choices`, yend = 0.86), color = ifelse(Data_1.1$`Country name` %in% c("Chile"), "yellow2", "skyblue3"), size = ifelse(Data_1.1$`Country name` %in% c("Chile"), 1.8, 1.5)) +
  coord_flip() +
  theme_elegante() +
  labs(title = "",
       caption = "",
       subtitle = "") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(size = 3.2, color = "steelblue4") +
  geom_point(size = 1.5, color = "white") +
  scale_y_continuous(breaks = c(0.86, 0.88, 0.90, 0.92, 0.94, 0.96, 0.98)) +
  geom_text(aes(x = Data_1.1$`Country name`, y = Data_1.1$`Freedom to make life choices`, label = round(Data_1.1$`Freedom to make life choices`, digits = 3)), hjust = -.42, nudge_x = 0, color = "black", size = 2.8)

# Parte 2
Data_1.2 %>%
  arrange(`Freedom to make life choices`) %>%
  mutate(`Country name` = factor(`Country name`, levels = `Country name`)) %>%
  ggplot(aes(x = `Country name`, y = `Freedom to make life choices`)) +
  geom_segment(aes(x = `Country name`, xend = `Country name`, y = `Freedom to make life choices`, yend = 0.74), color = ifelse(Data_1.2$`Country name` %in% c("Israel"), "yellow2", "skyblue3"), size = ifelse(Data_1.2$`Country name` %in% c("Israel"), 2, 1.5)) +
  coord_flip() +
  theme_elegante() +
  labs(title = "",
       caption = "",
       subtitle = "") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_blank(),
        axis.ticks.y = element_blank())+
  geom_point(color = ifelse(Data_1.2$`Country name` %in% c("Israel"), "yellow2", "steelblue4"), size = ifelse(Data_1.2$`Country name` %in% c("Israel"), 3.5, 3.2)) +
  geom_point(color = "white", size = 1.5) +
  scale_y_continuous(breaks = c(0.74, 0.76, 0.78, 0.80, 0.82, 0.84, 0.86)) +
  geom_text(aes(x = Data_1.2$`Country name`, y = Data_1.2$`Freedom to make life choices`, label = round(Data_1.2$`Freedom to make life choices`, digits = 3)), hjust = -.42, nudge_x = 0, color = "black", size = 2.8)

# Probe con paises al azar hasta que marque color en Chile. Por alguna razon los paises los registra mal.

# Continente Latinoamericano ----

Data_1.3 <- WHR_2020 %>%
  select("Country name", "Freedom to make life choices", "Regional indicator")

Data_1.4 <- Data_1.3 %>%
  filter(`Regional indicator` == "Latin America and Caribbean")

# Cambio de nombre a español (Paises) ----
{ Data_1.4$`Country name`[Data_1.4$`Country name` == "Haiti"] = "Haití"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Brazil"] = "Brasil"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Peru"] = "Perú"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Trinidad and Tobago"] = "Trinidad y Tobago"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Dominican Republic"] = "República Dominicana"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Panama"] = "Panamá"
Data_1.4$`Country name`[Data_1.4$`Country name` == "Panama"] = "Panamá"
}

# Grafico
Data_1.4 %>%
  arrange(`Freedom to make life choices`) %>%
  mutate(`Country name` = factor(`Country name`, levels = `Country name`)) %>%
  ggplot(aes(x = `Country name`, y = `Freedom to make life choices`)) +
  geom_segment(aes(x = `Country name`, xend = `Country name`, y = `Freedom to make life choices`, yend = 0.52), color = ifelse(Data_1.4$`Country name` %in% c("Uruguay"), "yellow2", "skyblue3"), size = ifelse(Data_1.4$`Country name` %in% c("Uruguay"), 2, 1.5)) +
  coord_flip() +
  theme_elegante() +
  labs(title = "",
       caption = "",
       subtitle = "") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(color = ifelse(Data_1.4$`Country name` %in% c("Uruguay"), "yellow2", "steelblue4"), size = ifelse(Data_1.4$`Country name` %in% c("Uruguay"), 3.5, 3.2)) +
  geom_point(color = "white", size = 1.5) +
  scale_y_continuous(breaks = c(0.52, 0.54, 0.56, 0.58, 0.60, 0.62, 0.64, 0.66, 0.68, 0.70, 0.72, 0.74, 0.76, 0.78, 0.80, 0.82, 0.84, 0.86, 0.88, 0.90, 0.92, 0.94)) +
  geom_text(aes(x = Data_1.4$`Country name`, y = Data_1.4$`Freedom to make life choices`, label = round(Data_1.4$`Freedom to make life choices`, digits = 3)), hjust = -.42, nudge_x = 0, color = "black", size = 2.8)


## Graficos y visualizacion de datos
### Dragon Ball
### Autor: Naim Manriquez

## Instalamos librerias

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("readxl")
install.packages("extrafont")

# Cargamos librerias
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
library(extrafont)

font_import()
head(fonts())
loadfonts(device = "win", quiet = TRUE) 

## Otra forma de cargar librerias

# install.packages("pacman")
# library(pacman)
## p_load(tidyverse, dplyr, ggplot2, ggthemes, readxl, extrafont)

## Vamos a trabajar sin notacion cientifica

options(scipen=999)

## Cargamos bases de datos

dragonball <- read_csv("Dragon_Ball_Data_Set.csv")

## Exploramos la base con str() de R base que significa structure

str(dragonball)

## Hacemos lo mismo con glimpse

glimpse(dragonball)

## ¿Cuantos personajes hay por saga/pelicula y serie? 

table(dragonball$Saga_or_Movie)

table(dragonball$Dragon_Ball_Series)

## Filtrando datos (solo quiero a los de Dragon Ball Z)

dragonballz <- dragonball %>%    
  filter (Dragon_Ball_Series == "Dragon Ball Z")

## Y ahora con los de Dragon Ball GT 

dragonballgt <- dragonball %>%    
  filter (Dragon_Ball_Series == "Dragon Ball GT")

## ¿Cual es el poder mas alto por serie? (primero convertimos de caracter a numerico)

dragonballz$Power_Level <- as.numeric(dragonballz$Power_Level)

summary(dragonballz$Power_Level)

## Grafica de nivel de poderes en Dragon Ball Z (saga de trunks)

dbz <- dragonballz %>%
  filter(str_detect(Saga_or_Movie, "Trunks Saga")) %>%
  ggplot(aes(x= Power_Level, y= Character)) +
  geom_col() 

dbz

## Grafica de nivel de poderes en Dragon Ball Z (saga saiyajin)

dbz1 <- dragonballz %>%
  filter(str_detect(Saga_or_Movie, "Saiyan Saga")) %>%
  ggplot(aes(x= Power_Level, y= Character)) +
  geom_col() 

dbz1


## Grafico mas chevere

dragonballz %>%
  filter(str_detect(Saga_or_Movie, "Trunks Saga")) %>%
  ggplot(aes(x= Power_Level, y= Character, fill = Power_Level)) +
  geom_col() +
  scale_fill_gradient(low = "#DEDEDE", high = "#e50914")+ #color de las columnas
  labs(title = "Dragon Ball Z",  #Título principal
       subtitle = "Nivel de poder/ki por personaje", 
       caption = "github \n @naimmanriquez") +
  theme_minimal() +
  theme(axis.text=element_text(colour="#000000"), 
        plot.title = element_text(color = "#e50914", size = 15, #color y tamaño título
                      hjust = .5, face = "bold", family = "Georgia"),
        plot.subtitle = element_text(color = "#000000", hjust = .5,
                                     face = "bold")) #título centrado negrita

## Gama de colores en R en https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

## Filtramos por poder de pelea entre 300 a 900 y por saga

dragonballz %>%
filter(Power_Level %in% c(300:900))%>%
  ggplot(aes(x = Power_Level, y = Character)) +
  geom_col()   

dragonballz %>%
  filter(Power_Level %in% c(300:900))%>%
  ggplot(aes(x = Power_Level, y = Character)) +
  geom_point(aes(shape= Saga_or_Movie, colour = Saga_or_Movie), size = 4)

## Creamos una base para los de menor poder

menorpoder <- dragonballz %>%
  filter(Power_Level %in% c(300:900))

## Para graficos interactivos
install.packages("plotly")

library(plotly)

plot_ly(menorpoder, x = ~Power_Level, y = ~Character, type = "scatter",
        symbol = ~factor(Saga_or_Movie)) %>% 
  layout(title = "Niveles de Ki",
         xaxis = list(title = "Poder"), 
         yaxis = list(title = "Personaje"))

## Creamos una base para los mas poderosos

maspoder <- dragonballz %>%
  filter(Power_Level %in% c(100000:1000000))

plot_ly(maspoder, x = ~Power_Level, y = ~Character, type = "scatter",
        symbol = ~factor(Saga_or_Movie)) %>% 
  layout(title = "Niveles de Ki",
         xaxis = list(title = "Poder"), 
         yaxis = list(title = "Personaje"))


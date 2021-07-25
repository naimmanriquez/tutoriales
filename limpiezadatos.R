## Limpieza de datos 
## Dragon Ball
## Autor: Naim Manriquez

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

## Exploramos las variables con la funcion glimpse

dragonball %>% glimpse()

dragonball %>% tail()

## Vamos a buscar solo a Vegeta

dball_data_vegeta <- dragonball %>%
  filter(str_detect(Character, regex("Vegeta", ignore_case = TRUE))) 

## Vemos nuestra nueva base de solo Vegeta

dball_data_vegeta %>% head()

#### Vamos a quitarle los decimales 

dball_vegeta_limpio <- dball_data_vegeta %>%
  mutate_at("Power_Level", ~str_remove_all(., ",")) %>%
  mutate_at("Power_Level", ~as.numeric(.)) 

## Exploramos de nuevo los datos

dball_vegeta_limpio %>% tail()

## Quitamos a Gohan (que aparecian relacionados)

dballvegeta <- dball_vegeta_limpio %>%
  filter(!str_detect(Character, "Gohan")) # Quitamos ese Gohan

# Nos quedamos con los vegeta

Vegeta_grouped <- dballvegeta %>%
  group_by(Character) %>%
  summarise_at(vars(Power_Level), ~mean(.)) %>%
  arrange(desc(Power_Level))  # Mostrar en manera descendente

Vegeta_grouped

## Estadisticos descriptivos

Vegeta_grouped %>%
  summary()

### Grafica

Vegeta_grouped %>%
  mutate(Power_Index = log(Power_Level)) %>% # Transformamos en logaritmo
  ggplot(aes(x = reorder(Character, Power_Level),
             y = Power_Index,
             fill = Character)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  geom_text(aes(y = Power_Index,
                label = round(Power_Index, 1),
                hjust = -.2),
            colour = "#FFFFFF") +
  ggtitle("Niveles de poder de Vegeta", subtitle = "Por serie y saga") +
  theme(plot.background = element_rect(fill = "grey20"),
        text = element_text(colour = "#FFFFFF"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="#FFFFFF", face="bold", size=20),
        axis.line = element_line(colour = "#FFFFFF"),
        legend.position = "none",
        axis.title = element_text(colour = "#FFFFFF", size = 12),
        axis.text = element_text(colour = "#FFFFFF", size = 12)) +
  ylab("Nivel de Pelea (logaritmo)") +
  xlab(" ")




Vegeta_grouped %>%
  mutate(Power_Index = log(Power_Level)) %>% # Transformamos en logaritmo
  ggplot(aes(x = reorder(Character, Power_Level),
             y = Power_Index)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  geom_text(aes(y = Power_Index,
                label = round(Power_Index, 1),
                hjust = -.2),
            colour = "#000000") + ## Color negro con el indice
  ggtitle("Niveles de poder de Vegeta", subtitle = "Por serie y saga") +
  theme(plot.background = element_rect(fill = "ivory"), ## color para el fondo
        text = element_text(colour = "green4")) + # color para las letras encabezado y ejes
  ylab("Nivel de Pelea (logaritmo)") +
  xlab(" ") 


## Hacemos ahora por saga

dballvegeta %>%
  filter(Character == "Vegeta") %>%
  mutate(Power_Index = log(Power_Level)) %>% # Log transform Power Levels
  group_by(Saga_or_Movie) %>%
  summarise(Power_Index = mean(Power_Index)) %>%
  ggplot(aes(x = reorder(Saga_or_Movie, Power_Index),
             y = Power_Index)) +
  geom_col(fill = "#F85B1A") +
  theme_minimal() +
  geom_text(aes(y = Power_Index,
                label = round(Power_Index, 1),
                vjust = -.5),
            colour = "#FFFFFF") +
  ggtitle("Niveles de pelea de Vegeta", subtitle = "Por Saga y Pelicula") +
  scale_y_continuous(limits = c(0, 30)) +
  theme(plot.background = element_rect(fill = "grey20"),
        text = element_text(colour = "#FFFFFF"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="#FFFFFF", face="bold", size=20),
        plot.subtitle = element_text(colour="#FFFFFF", face="bold", size=12),
        axis.line = element_line(colour = "#FFFFFF"),
        legend.position = "none",
        axis.title = element_text(colour = "#FFFFFF", size = 10),
        axis.text.y = element_text(colour = "#FFFFFF", size = 8),
        axis.text.x = element_text(colour = "#FFFFFF", size = 8, angle = 45, hjust = 1)) +
  ylab("Nivel de pelea (logaritmo)") +
  xlab(" ")



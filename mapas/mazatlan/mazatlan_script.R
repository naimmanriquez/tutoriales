########################################################
#Mapa de polígonos | Bicicletas en Mazatlan nivel AGEB
#Elaborado por: Naim Manriquez Garcia 
#               Twitter: @naimmanriquez
#               Github: @naimmanriquez
########################################################

##############
#Configuración
rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr,gmodels)


##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
#hcl_palettes(plot = TRUE)

##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_sin <- st_read("sin_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_sin$mun <- substr(mun_sin$CVEGEO, 3, 5)
#Filtrar a Mazatlan
mun_sin <- mun_sin %>% 
  filter(mun=="012")

##############
#AGEBs
ageb_sin <- st_read("sin_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_sin$mun <- substr(ageb_sin$CVEGEO, 3, 5)
#Filtrar a ZMG
ageb_sin <- ageb_sin %>% 
  filter(mun=="012")


#Seleccionar variables de interés
ageb_sin_2 <- ageb_sin %>% 
  dplyr::select(CVEGEO, mun)

#Leer los datos complementarios
datos_complemen <- read_dta("sin_ageb_urb.dta")

#Unir bases
ageb_complemen<-merge(x=ageb_sin_2,y=datos_complemen,by="CVEGEO")

#Seleccionar variables de interés
ageb_sin_2 <- ageb_complemen %>% 
  dplyr::select(CVEGEO, mun, y, z, analf2020, bici, isinsal2020)

#Mapa simpl
ggplot () +
  theme_void()+
  geom_sf(data = ageb_sin_2, aes(fill = bici), lwd = 0) +
  scale_fill_gradientn(colors = viridis::viridis(20)) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Hogares con bicicleta en la ciudad de Mazatlán", subtitle = "Cantidad de bicicletas") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Bicicletas") +
  theme(text = element_text(family = "Verdana", color = "white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(5,5,5,5), units = "point"),
        panel.background = element_rect(fill = "black", linetype = "blank"),
        plot.background = element_rect(fill = "black", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2010.",
         fill = "#")

# Datos para nombres de municipios
mun_sin <- cbind(mun_sin, st_coordinates(st_centroid(mun_sin)))


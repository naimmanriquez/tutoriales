# 1. Removemos notación científica
options(scipen = 999)

# 2. Caracteres usados en el idioma español
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

# Instalamos librerias en caso de no tenerlas instaladas
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

## Cargamos librerias
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

## Cargamos las bases de datos
## La funcion <- sirve para llamar objetos y nombrarlos
## En este caso llamamos la base de datos de jugadores y lo llamamos fifa

fifa <- read_csv("players_20.csv")

## Con las funciones str y glimpse vemos la estructura de los datos
str(fifa)
glimpse(fifa)

## Para saber el nombre de todas mis variables usamos names()
names(fifa)

## ¿Cuantos jugadores hay por pais? 

table(fifa$nationality)

## Filtrando datos (solo quiero a los de Mexico)

jugadores_mexico <- fifa %>%    
  filter (nationality == "Mexico")

# ¿Que club tiene mas mexicanos?
table(jugadores_mexico$club)

## Comparamos estatura con peso

ggplot(jugadores_mexico, aes(x=weight_kg, y=height_cm)) +
  geom_point(size=2, shape=22)

## Shape se refiere al tipos de puntos: http://www.sthda.com/english/wiki/ggplot2-point-shapes

## Libreria ggthemes es para cambiar temas de graficos
library(ggthemes)

# Un tema de Stata
ggplot(jugadores_mexico, aes(x=weight_kg, y=height_cm)) +
  geom_point(size=2, shape=22) +
  theme_stata()

# Un tema clasico
ggplot(jugadores_mexico, aes(x=weight_kg, y=height_cm)) +
  geom_point(size=2, shape=22) +
  theme_classic()

## Hacemos algunos estadisticos descriptivos
## Edad
summary(jugadores_mexico$age)

## Altura
summary(jugadores_mexico$height_cm)

## Peso
summary(jugadores_mexico$weight_kg)

## Quienes son esos mas bajos de estatura

estaturabaja <- jugadores_mexico %>%
  filter(height_cm %in% c(159:163))

table(estaturabaja$short_name)


## quienes son los mas altos de estatura

estaturaalta <- jugadores_mexico %>%
  filter(height_cm %in% c(170:180))

table(estaturaalta$short_name)

## Tablas de frecuencias

install.packages('epiDisplay')
library(epiDisplay)
tab1(jugadores_mexico$height_cm, cum.percent = TRUE)

library(here)
library(haven)
library(survey)
library(tidyverse)

# Integración de BD ENIGH 2020
CONCENTRADO <- read_sav("concentradohogar.sav")
HOGARES <- read_sav("hogares.sav")
VIVIENDAS <- read_sav("viviendas.sav")

#Uniendo bases de hogares
FULL.H<-merge(HOGARES,CONCENTRADO,by=c("folioviv","foliohog"),all.x=T)
#Uniendo componentes de vivienda
BD<-merge(FULL.H,VIVIENDAS,by=c("folioviv"),all.x=T)

# Creando variables NSE ---------------------------------------------------
BD$autos<-BD$num_auto+BD$num_van+BD$num_pickup
BD$internet<-as.numeric(BD$conex_inte==1)

BD$internet <- NA
BD$internet <- ifelse(BD$conex_inte == "1", 1, 0)

BD$edu_jefe<-
  factor(BD$educa_jefe,levels=c("01","02","03","04","05","06","07","08","09","10","11"),
         labels=c("Sin instrucción","Preescolar","Primaria incompleta","Primaria
completa","Secundaria incompleta","Secundaria completa","Preparatoria
incompleta","Preparatoria completa","Profesional incompleta","Profesional
completa","Posgrado"))


#Transformación logística, sumando una unidad para evitar ceros
BD$ln_ing<-log(BD$ing_cor+1)


# Cálculos NSE ACTUALIZADO ------------------------------------------------
#Puntos Educación
BD$puntos_educa<-0
BD$puntos_educa[is.na(BD$educa_jefe)]<-NA
BD$puntos_educa[BD$educa_jefe=="03"]<-6
BD$puntos_educa[BD$educa_jefe=="04"]<-11
BD$puntos_educa[BD$educa_jefe=="05"]<-12
BD$puntos_educa[BD$educa_jefe=="06"]<-18
BD$puntos_educa[BD$educa_jefe=="07"]<-23
BD$puntos_educa[BD$educa_jefe=="08"]<-27
BD$puntos_educa[BD$educa_jefe=="09"]<-36
BD$puntos_educa[BD$educa_jefe=="10"]<-59
BD$puntos_educa[BD$educa_jefe=="11"]<-85

#Puntos Baños
BD$puntos_bano<-47
BD$puntos_bano[is.na(BD$bano_comp)]<-NA
BD$puntos_bano[BD$bano_comp==0]<-0
BD$puntos_bano[BD$bano_comp==1]<-24

#Puntos Autos
BD$puntos_autos<-43
BD$puntos_autos[is.na(BD$autos)]<-NA
BD$puntos_autos[BD$autos==0]<-0
BD$puntos_autos[BD$autos==1]<-22

#Puntos Internet
BD$puntos_internet<-0
BD$puntos_internet[is.na(BD$conex_inte)]<-NA
BD$puntos_internet[BD$internet==1]<-32

#Puntos Ocupación
BD$puntos_ocupados<-61
BD$puntos_ocupados[is.na(BD$ocupados)]<-NA
BD$puntos_ocupados[BD$ocupados==0]<-0
BD$puntos_ocupados[BD$ocupados==1]<-15
BD$puntos_ocupados[BD$ocupados==2]<-31
BD$puntos_ocupados[BD$ocupados==3]<-46

#Puntos Cuartos
BD$puntos_cuartos<-32
BD$puntos_cuartos[is.na(BD$cuart_dorm)]<-NA
BD$puntos_cuartos[BD$cuart_dorm==0]<-0
BD$puntos_cuartos[BD$cuart_dorm==1]<-8
BD$puntos_cuartos[BD$cuart_dorm==2]<-16
BD$puntos_cuartos[BD$cuart_dorm==3]<-24

#Suma Puntajes
BD$puntos_NSE_NUEVO <- BD$puntos_educa+BD$puntos_bano+BD$puntos_autos+BD$puntos_internet+BD$puntos_ocupados+BD$puntos_cuartos
BD$NSE_NUEVO<-cut(BD$puntos_NSE_NUEVO,breaks=c(0,47,94,115,140,167,201,300))
BD$NSE_NUEVO<-factor(BD$NSE_NUEVO,
                     levels=c("(0,47]","(47,94]","(94,115]","(115,140]","(140,167]","(167,201]","(201,300]")
                     ,labels=c("E","D","D+","C-","C","C+","A/B")
)

# # Estimaciones --------------------------------------------------------
# #Se define el diseño de muestra compleja utilizado en la ENIGH
# design.ENIGH<-svydesign(id=~upm.x,strata=~est_dis.y,weights=~factor.y,data = BD)
# #Porcentajes Nacionales Regla AMAI Actualizada
# svytable(~NSE_NUEVO,design.ENIGH,Ntotal=100,na.action=na.pass,exclude=NULL)

## Para Sinaloa
BD_sinaloa <- filter(BD, ubica_geo.y >= "25001" & ubica_geo.y <= "25018")
#Se define el diseño de muestra compleja utilizado en la ENIGH
design.ENIGH<-svydesign(id=~upm.x,strata=~est_dis.y,weights=~factor.y,data = BD_sinaloa)
#Porcentajes Nacionales Regla AMAI Actualizada
svytable(~NSE_NUEVO,design.ENIGH,Ntotal=100,na.action=na.pass,exclude=NULL)

## Algunas cosas de gastos de los hogares sinaloenses
## Gasto en alimentos fuera del hogar
tapply(BD_sinaloa$ali_fuera, BD_sinaloa$NSE_NUEVO, mean)
## Gasto en alimentos dentro del hogar
tapply(BD_sinaloa$ali_dentro, BD_sinaloa$NSE_NUEVO, mean)
## Gasto de los hogares sinaloenses
tapply(BD_sinaloa$gasto_mon, BD_sinaloa$NSE_NUEVO, mean)

gastos_sinaloenses <- data.frame(tapply(BD_sinaloa$gasto_mon, BD_sinaloa$NSE_NUEVO, mean))
names(gastos_sinaloenses)[1] <- "gasto_trimestral"
gastos_sinaloenses$nse = NA
gastos_sinaloenses$nse = c("E", "D", "D+", "C-", "C", "C+", "A/B")

library(RColorBrewer)
library(ggplot2)
library(extrafont)

gastos_sinaloenses=gastos_sinaloenses[order(gastos_sinaloenses$nse),]
gastos_sinaloenses$nse=factor(gastos_sinaloenses$nse,levels=gastos_sinaloenses$nse)

gastos_sinaloenses %>% 
  ggplot() + 
  geom_bar(aes(gasto_trimestral, nse), fill = "grey", stat = "identity") + 
  geom_bar(data = gastos_sinaloenses %>% filter(nse %in% c("A/B", "E")), 
           aes(gasto_trimestral, nse), fill = "#FF00D2", stat = "identity") + 
  geom_hline(aes(yintercept = 33371), colour = "skyblue3", size = 1) + 
  coord_flip() + 
  scale_x_continuous(breaks = seq(0, 73000, 10000), 
                     limits = c(0, 74000), 
                     expand = c(0, 0)) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(color = "gray20", size = 12, face = "italic"), 
        plot.caption = element_text(size = 10), 
        text = element_text(family = "Georgia", size = 15)) + 
  labs(title = "Gasto en bienes y servicios por NSE (en pesos)", 
       subtitle = "Gasto promedio de los hogares en Sinaloa", 
       x = NULL, y = NULL, 
       caption =  "Fuente: ENIGH, 2020. @data.viz.nm") ->> p1

p1



# ## Para Mazatlan
# BD_Mazatlan <- filter(BD, ubica_geo.y >= "25012")
# #Se define el diseño de muestra compleja utilizado en la ENIGH
# design.ENIGH<-svydesign(id=~upm.x,strata=~est_dis.y,weights=~factor.y,data = BD_Mazatlan)
# #Porcentajes Nacionales Regla AMAI Actualizada
# svytable(~NSE_NUEVO,design.ENIGH,Ntotal=100,na.action=na.pass,exclude=NULL)

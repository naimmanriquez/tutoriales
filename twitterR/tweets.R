## Caracteres usados en el idioma español

Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

# Cargamos librerias

library(tidyverse)
library(rtweet)

# Buscar tweets de un tema particular.

query <- '"Huracán Nora" lang:es'

# Busqueda
# (Solo se pueden descargar 15,000 tweets cada 15 minutos)
bd <- search_tweets(query,  # Busqueda
                    n = 2000, # Numero Maximo de Tweets
                    include_rts = FALSE, # Incluir Rts
                    retryonratelimit = TRUE)

## Stream tweets (Para Sinaloa)
stream_tweets <- stream_tweets(c(-111.599,
                                  21.555,
                                 -100.437,
                                  27.567),
                               timeout = 30) #segundos

## Si los queremos pasar a un excel
write.xlsx(bd, file = "twwetshuracan.xlsx")
write.xlsx(stream_tweets, file = "tweets_sinaloa.xlsx")








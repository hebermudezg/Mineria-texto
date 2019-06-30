install.packages("stringdist")

install.packages("udpipe")


library(httr) #sirve para hacer el request
library(jsonlite) #sirve para hacer el paring del response
library(lubridate) #sirve para trabajar con formatos de fecha
library(bit64) #sirve para trabajar con enteros largos
library(data.table)


# Genracion de las estddiaticas de resumen --------------------------------

library(ggplot2)
library(tidyverse) # carga la siguiente también
#library(dplyr)


# Para los paises
articles.flt.dt %>% 
  group_by(thread.country) %>% 
  count()

# Para los tipos de articulo

articles.flt.dt %>% 
  group_by(thread.site_type) %>% 
  count()

# Para ver los tipos de portales

articles.flt.dt %>% 
  group_by(thread.site) %>% 
  count()

# Visualización de numero de publicaciones  por dia

hola <-articles.flt.dt[,.N,c("thread.site_type","thread.site_full")]

table(hola$thread.site_full,hola$thread.site_type)

barplot(table(serie))

# Usar lubridate
library(lubridate)

serie <- date(articles.flt.dt$published)
serie
table(serie)
sort(table(serie))
plot(serie,type = "l")

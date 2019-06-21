library(httr)
library(jsonlite)


####Ejemplo 1####



####REQUEST####

#header parameters

#hay que sostituir los asteriscos con su propia key
pkey <- "d2152161-4eaf-463e-a036-4a49c7c5ca41"

phost <- "restcountries-v1.p.rapidapi.com"


#path parameter

ppath <- "/lang/es" 


#creando el http

http <- paste0("https://", phost, ppath)


#ejecutamos el request usando el metodo GET

response <- GET(http, 
                add_headers("X-RapidAPI-Host" = phost,
                            "X-RapidAPI-Key" = pkey))



########RESPONSE########

#ahora vamos a analizar la respuesta que nos di? el endpoint 

#responde status code

status_code(response)

http_error(response)


#response header

headers(response)


#response body raw en formato json

countries.text <- content(response, as = "text")

#efectuando el parsing del formato json usando jsonlite

countries <- fromJSON(countries.text)




library(data.table)
library(quanteda)

filename <- "YOUR PATH"
load(filename)



#####TOPICS#####


## categorias mas frecuentes
# la columna de las categorias en el data.table tiene una estructura particular 
# y hay que tratarla de forma espcifica
# igual por medio de una dtm podemos calcular facilmente las categorias frecuentes
View(articles.flt.dt)
# empezamos quedandonos con las solas filas que contienen categorias
ht <- articles.flt.dt[is.na(thread.site_categories)==F, "thread.site_categories"]
# la idea es poder collapsar la lista en una cadena de texto separada por espacios
thread.site_categories_string <- lapply(ht$thread.site_categories, 
                                             paste, collapse = " ")
thread.site_categories_string <- as.data.table(unlist(thread.site_categories_string))




# ahora si creo el corpus de las categorias
ht.corpus<-corpus(thread.site_categories_string$V1)
# la dtm
ht.dtm<-dfm(ht.corpus)
# las 15 categorias mas frecuentes
topht <- topfeatures(ht.dtm, 15)
topht <- topfeatures(ht.dtm, 30)
# este tipo de vector named no lo puedo tranformar directamente en data.table
# debo pasar por un data.frame
topht <- as.data.frame(topht)
# y ahora si en un data table recordandome de mantener los rownames
topht <- as.data.table(topht, keep.rownames = T)
topht

barplot(topht$topht, names.arg = topht$rn,las=2,cex.names = 0.75)


# Exportando a csv
write.csv(topht,file = "daticos1.csv")


#actualizar el conteo social
library(httr)

url<-"https://www.elespectador.com/copa-america-2019/prueba-superada-brasil-goleo-bolivia-en-su-debut-en-la-copa-america"
key<-"YOUR APIKEY"


http <- paste0("https://api.sharedcount.com/v1.0/?url=", url, "&apikey=", key)

response <- GET(http)

socialcount <- content(response)

socialcount$Facebook$total_count

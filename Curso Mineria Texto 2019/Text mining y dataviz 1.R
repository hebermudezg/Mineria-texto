#vamos a hacer un request al API de webhose sin wrapper

library(httr) #sirve para hacer el request
library(jsonlite) #sirve para hacer el paring del response
library(lubridate) #sirve para trabajar con formatos de fecha
library(bit64) #sirve para trabajar con enteros largos
library(data.table) #sirve para trabajar con big data

#vamos construyendo el request

#declaramos el endpoint

endpoint <- "http://webhose.io/filterWebContent"

#declaramos los parametros de query que van a ser
#las palabras a buscar dentro de los articulos, el token, el formato, el ordanamiento 
#la paginacion y el timestamp

#keyword
q.v <- "estadistica"
q.v

#token
apikey.webhose <- "fda0dba5-df49-4307-b420-9db26018797e"
apikey.webhose

#formato
format.v <- "json" 
format.v

#timestamp
#la composicion del tiemstamp no es inmediata. Debemos declarar que vamos a buscar 
#los últimos 30 días de datos pero en formato UNIX milliseconds
#para poderlo hacer primero encuentro la fecha actual

now <- Sys.time()
now

#luego le le quito 30 dias
monthago <- now - days(30)
monthago

#finalmente encuentro la fecha en milliseconds
#milliseconds es el tiempo calculado en fracciones de segundo desde el 1-1-1970
#por el tipo de timestamp de webhose necesito tenerlo en unidades
#siendo que los numeros grandes R lo escribe automaticamente en forma exponencial
#tengo que usar la funcion as.integer64 del paquete bit 

ts.v <- as.integer64(as.numeric(monthago)*1000, digits=15)
ts.v

#creando el http del request

http <- paste0(endpoint, "?q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v)
http

q.v_1 <- "estadistica colombia"
q.v_1 <- URLencode(q.v_1)
q.v_1

http1 <- paste0(endpoint, "?q=", q.v_1, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v)

#hacemos el request con metodo GET 

response <- GET(http1, 
                add_headers("Accept" = "text/plain"))


########RESPONSE########

#ahora vamos a analizar la respuesta que nos dió el endpoint 
#responde status code
status_code(response)
http_error(response)


#response header
headers(response)

#response body raw en formato json
articles.text <- content(response, as = "text", encoding = "UTF-8")
#response body como viene por defecto en httr
articles.parsed <- content(response, as = "parsed")
#efectuando el parsing del formato JSON usando jsonlite
articles <- fromJSON(articles.text)
#es bueno conocer tambien el parametro flatten que nos permite 
#crear una matriz sin data.frame nested
articles.flt <- fromJSON(articles.text, flatten = T)

#commparando el data.frame normal y el flatten
articles.df <- articles$posts
articles.flt.df <- articles.flt$posts


#complicamos la busqueda usando mas de una palabra de query
q.v <- "estadistica colombia"


#el http por como lo creamos tiene espacios blancos que un url no puede tener
#solucionamos el problema haciendo un encode de la query
q.v <- URLencode(q.v)


#volvemos a crear el http
http <- paste0(endpoint, "?q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v)


#volvemos a hacer el request 
response <- GET(http, 
                add_headers("Accept" = "text/plain"))


#responde status code
status_code(response)
#response header
headers(response)
#response body
articles.text <- content(response, as = "text", encoding = "UTF-8")
articles.flt <- fromJSON(articles.text, flatten = T)

#######paginacion

#si miramos los resultados totales disponibles nos damos cuenta que son mas de 100 
articles.flt$totalResults

#para poder descargar los demas tenemos que trabajar con paginacion
#para poderlo hacer tenemos que introducir tambien el parametro sort 
#por defecto los resultados estan ordenados por fecha de crawl que pero no nos permite de 
#trabajar con paginacion. El sort por relevancia por ejemplo si nos lo permite
#esta informacion la encontramos en la pagina https://docs.webhose.io/docs/get-parameters
sort.v <- "relevancy"
sort.v


#introduciendo el parametro de paginacion (por defecto la primera pagina es 0)
#esta informacion la encontramos en la pagina https://docs.webhose.io/docs/get-parameters
from.v <- 0

#vuelvo a crer el http con los nuevos parametros
http2 <- paste0(endpoint, "?q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v, "&sort=", sort.v,  "&from=", from.v)

response <- GET(http, 
                add_headers("Accept" = "text/plain"))



#responde status code
status_code(response)

#response header
headers(response)

#response body raw en formato json
articles.text <- content(response, as = "text", encoding = "UTF-8")
articles.flt <- fromJSON(articles.text, flatten = T)

articles.flt.df <- articles.flt[["posts"]]
articles.flt.tr <- articles.flt[["totalResults"]]
articles.flt.mra <- articles.flt[["moreResultsAvailable"]]
articles.flt.mra

#para descargar los demas articulos tengo que crear un bucle descargando todas las paginas
#se puede hacer de varuas formas aprovechando de la informacion que tenemos en la descarga
#podemos aprovechar de los moreRsultasAvailable pero hay un objeto mas efectivo

articles.flt.next <- articles.flt[["next"]]
articles.flt.next

#next es un parametro del body que recurre muchas veces en apis con paginacion
#a veces nos proporciona el codigo de la pagina siguiente
#en este caso nos da directamente el http para el request de la siguiente pagina

articles.flt.dt <- as.data.table(articles.flt.df)
articles.flt.dt

#inicializo el parametro de salida
nout <- 100

while(nout == 100){
  
  http <- paste0("http://webhose.io", articles.flt.next)
  
  response <- GET(http, 
                  add_headers("Accept" = "text/plain"))
  
  articles.text.tmp <- content(response, as = "text", encoding = "UTF-8")
  articles.flt.tmp <- fromJSON(articles.text.tmp, flatten = T)
  
  articles.flt.dt.tmp <- as.data.table(articles.flt.tmp[["posts"]])
  articles.flt.next <- articles.flt.tmp[["next"]]
  nout <- nrow(articles.flt.dt.tmp)
  
  l <- list(articles.flt.dt, articles.flt.dt.tmp)
  articles.flt.dt <- rbindlist(l, use.names = T)
  
  articles.flt.mra <- articles.flt.tmp[["moreResultsAvailable"]]
  hd <- headers(response)
  apicall <- hd[["x-webhose-requests-left"]]
  msg <- paste0("Articulos restantes: ", articles.flt.mra, " - Requests restantes: ", apicall)
  print(msg)

}
# esto realiza un append por filas con la coincidencia entre colomnas, las columnas adiccionales se agragn a la derecha y en aquelos appends que no las tienen queda información vacía.

#EJERCICIO 1

####################################
########trabjar con Big data########
####################################
#############data.table#############
####################################

#inspeccionamos la descarga
#usando la libreria data.table 

## selecciones de columnas multiplas
articles.flt.dt[, list(uuid, crawled)]
articles.flt.dt[, .(uuid, crawled)] #esto es equivalente
articles.flt.dt[, c("uuid", "crawled")] #esto es como se haría con data frames
articles.flt.dt[, c(uuid, crawled)] #esto con data frames daría error


## operaciones con columnas
# crear una nueva columna vacia
articles.flt.dt[, new_col := NA]
articles.flt.dt$new_col <- NA #forma alterna al estilo data frame

# eliminar una columna
articles.flt.dt[, new_col := NULL]
articles.flt.dt$new_col <- NULL


# creamos una columna mediante funcion
# en este caso creamos una columna cambiando el formato de chr a POSIXct
articles.flt.dt[, published_utc := ymd_hms(published)]

# cambiando el timezone UTC (Londres) al de Colombia
articles.flt.dt[, published_col := ymd_hms(published, tz = "America/Bogota")]

# visualizamos la columna original con timezone UTC (Londres) 
# y la columna con timezone de Colombia 
articles.flt.dt[, list(published_utc, published_col)]

# operaciones sobre columnas 
# promedio y maximo de los share de facebook
articles.flt.dt[, mean(thread.social.facebook.shares, na.rm = T)]
articles.flt.dt[, max(thread.social.facebook.shares, na.rm = T)]

# operaciones sobre columnas y una seleccion de filas 
# promedio del rating de los articulos en espanol 
articles.flt.dt[language=="spanish", mean(thread.social.facebook.shares)]

# operaciones entre columnas
# suma de favoritos y retweet
articles.flt.dt[, fb := thread.social.facebook.shares + thread.social.facebook.likes]
articles.flt.dt[, list(fb, thread.social.facebook.shares, thread.social.facebook.likes)]

# operacion de ordenamiento por capitalizacion de mercado
# descendiente
articles.flt.dt <- articles.flt.dt[order(-fb)]
View(articles.flt.dt)

# Operador especial .N
# este operador sirve para hacer conteos
# numero de filas
articles.flt.dt[, .N]

#numero de filas que cumplen con una condicion 
articles.flt.dt[language=="spanish", .N]

# ahora utilizamos el by para agrupar
# numero de articulos por periodico
articles.flt.dt[, .N, by = thread.site_full]

# operador .I crea un id
articles.flt.dt[, id := .I]

# operador .GRP crea id de grupo
articles.flt.dt[, id := .GRP, by = language]



#########EJERCICIO 2#########



####################################
########trabjar con Big data########
####################################
##############tidyverse#############
####################################

library(magrittr)

#logica de programacion pipe %>%

x<- 4


log(x)

x %>% 
  log()



round(log(x), 1)

x %>% 
  log() %>% 
  round(1)



exp(round(log(x), 1))

x %>% 
  log() %>% 
  round(1) %>% 
  exp()




library(dplyr) #libreria tidyverse que trabaja con manipulacion de dataframes


#seleccionar columnas
articles.flt.sl  <- articles.flt.dt %>% 
  select(uuid, crawled)


#filtrar segun el valor de un campo
articles.flt.sl <- articles.flt.dt %>% 
  filter(language == "spanish")

head(articles.flt.sl$language)


#crear columnas nuevas desde otras columnas
articles.flt.sl <- articles.flt.dt %>%
  mutate(published_col = ymd_hms(published, tz = "America/Bogota"))

head(articles.flt.sl[, c("published", "published_col")])


#conteo de filas
articles.flt.count <- articles.flt.dt %>% 
  count()

#frecuencias
articles.flt.count <- articles.flt.dt %>% 
  group_by(language) %>%
  count()


#agregar valores
articles.flt.sum <- articles.flt.dt %>% 
  summarise(shares_tot = mean(thread.social.facebook.shares))


#agregar valores apgrupandolos por las modalidades un campo
articles.flt.sum <- articles.flt.dt %>%
  group_by(thread.site_full) %>%
  summarise(result_tot = mean(thread.social.facebook.shares))

articles.flt.sum


#ordenar las columns
articles.flt.sl <- articles.flt.dt %>%
  arrange(uuid)



####EJERCICIOS####



#ahora que estuvimos explorando el archivo empezamos a mejorar el query
#por ejemplo seleccionando solo los articulos de idioma espanol


#los parametros que webhose propone en "+add filter" no son los clasicos parametros de query
#usando la herramienta  "Define the query" se pueden notar que todos esos filtros terminan
#en el parametro q

q.v <- "estadistica colombia"
language.v <- "spanish"

q.v <- paste0(q.v, " language:", language.v)

q.v <- URLencode(q.v, reserved = T, repeated = T)


#vuelvo a crer el http con los nuevos parametros
http <- paste0(endpoint, "?q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v, "&sort=", sort.v,  "&from=", from.v)


response <- GET(http, 
                add_headers("Accept" = "text/plain"))



#response body raw en formato json
articles.text <- content(response, as = "text", encoding = "UTF-8")
articles.flt <- fromJSON(articles.text, flatten = T)

articles.flt.dt <- as.data.table(articles.flt[["posts"]])
articles.flt.dt[, .N, by = language]


articles.flt.next <- articles.flt[["next"]]
articles.flt.dt <- as.data.table(articles.flt.df)

nout <- 100

while(nout == 100){
  
  http <- paste0("http://webhose.io", articles.flt.next)
  
  response <- GET(http, 
                  add_headers("Accept" = "text/plain"))
  
  articles.text.tmp <- content(response, as = "text", encoding = "UTF-8")
  articles.flt.tmp <- fromJSON(articles.text.tmp, flatten = T)
  
  articles.flt.dt.tmp <- as.data.table(articles.flt.tmp[["posts"]])
  articles.flt.next <- articles.flt.tmp[["next"]]
  nout <- nrow(articles.flt.dt.tmp)
  
  l <- list(articles.flt.dt, articles.flt.dt.tmp)
  articles.flt.dt <- rbindlist(l, use.names = T)
  
  articles.flt.mra <- articles.flt.tmp[["moreResultsAvailable"]]
  hd <- headers(response)
  apicall <- hd[["x-webhose-requests-left"]]
  msg <- paste0("Articulos restantes: ", articles.flt.mra, " - Requests restantes: ", apicall)
  print(msg)
  
}

filename <- ""
save(articles.flt.dt, file = filename)




###EJERCICIOS













#trabajar con wrapper webhose

library(webhose)

articles.ls <- filter_posts(query = q.v, ts = ts.v, token = apikey.webhose, 
                            sort = "published", order = "asc", from = 0)


#trabajr con newsapi

#endpoint
endpoint.v <- "https://newsapi.org/v2/everything?"

#parametros de query
query.v <- "estadistica"
language.v <- "es"
from.v <- "2019-05-00"
apikey.newsapi <- "be2aad8467654c118db761246c51159d"
pageSize.v <- "100"


http <- paste0(url.v, "q=", query.v, "&apiKey=", apikey.newsapi, 
               "&from=", from.v, "&language=", language.v, "&pageSize=", pageSize.v)

response <- GET(http)

status_code(response)

articles.text <- content(response, as = "text")

articles <- fromJSON(articles.text)

articles.df <- articles$articles

articles$totalResults
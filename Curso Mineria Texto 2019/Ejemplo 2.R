#vamos a hacer un request al API de webhose sin wrapper

library(httr) #sirve para hacer el request
library(jsonlite) #sirve para hacer el paring del response
library(lubridate) #sirve para trabajar con formatos de fecha
library(bit64) #sirve para trabajar con enteros largos
library(data.table) #sirve para trabajar con big data

#vamos construyendo el request

#declaramos el endpoint

endpoint1 <- "http://webhose.io/filterWebContent"

#declaramos los parametros de query que van a ser
#las palabras a buscar dentro de los articulos, el token, el formato, el ordanamiento 
#la paginacion y el timestamp

#keyword
q.v <- "Transformacion Digital AND Colombia"

apikey.webhose <- "fda0dba5-df49-4307-b420-9db26018797e"

format.v <- "json" 
q.v <- URLencode(q.v) 

# todo esto se evita al usar:
# endpoint <- "http://webhose.io/filterWebContent?token=fda0dba5-df49-4307-b420-9db26018797e&format=json&sort=crawled&q=stock%20market%20language%3Aenglish"

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

http <- paste0(endpoint1, "?q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v)
http

#hacemos el request con metodo GET 

response <- GET(http, 
                add_headers("Accept" = "text/plain"))

#ahora vamos a analizar la respuesta que nos di?? el endpoint 
#responde status code
status_code(response)
http_error(response)

#response header
headers(response)

articles.parsed <- content(response, as = "parsed")

articles <- fromJSON(articles.text)

#es bueno conocer tambien el parametro flatten que nos permite 
#crear una matriz sin data.frame nested

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

# esto realiza un append por filas con la coincidencia entre colomnas, las columnas adiccionales se agragn a la derecha y en aquelos appends que no las tienen queda informaci??n vac??a.

# Operador especial .N
# este operador sirve para hacer conteos
# numero de filas
articles.flt.dt[, .N]

#numero de filas que cumplen con una condicion 
articles.flt.dt[language=="spanish", .N]

# ahora utilizamos el by para agrupar
# numero de articulos por periodico
articles.flt.dt[, .N, by = thread.site_type]
articles.flt.dt[, .N, by = thread.country]
articles.flt.dt[, .N, by = c("thread.country","thread.site_type")]

library(devtools)
install_github("hrbrmstr/webhose")

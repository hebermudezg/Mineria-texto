rm(list=ls())


#vamos a hacer un request al API de webhose sin wrapper

library(data.table) #sirve para trabajar con big data
library(ggplot2) #sirve para hacer graficos
library(stringr) #sirve para manipular texto


filename <- "C:/Users/Cabala Skills/Dropbox/Main/Freelo/Colombia/2019/Sce/Articles.Rdata"
load(filename)


#####EJERCICIO#####



## MANIPULACION BASICA DE DATOS DE TEXTO

text1 <- "#TuMundoHoy Alerta máxima por constantes movimientos en montaña de Hidroituango."


## enconding
## esta parte es importante mas que todo para los Windows
# quiero ver cual es el encoding de los caracteres
Encoding(text1)

# pasar todo a UTF-8 que es el enconding base de R
text1 <- str_conv(text1, "UTF-8")


# buscar y reemplazar
# buscamos una cadena de texto con la funcion grepl
grepl(pattern = "Hidroituango", x = text1)
grepl("hidroituango", text1)

# remplazamos una cadena de texto con la funcion gsub
text1
gsub(pattern = "#", replacement = "", x = text1)
gsub(pattern = "constantes", replacement = "continuos", x = text1)

## concatenar
# concatenamos dos cadenas de texto 
text2 <- "Vuelve a elevarse la alerta roja en Tarazá y Cáceres."
text2 <-str_conv(text2,  "UTF-8")

paste(text1, text2)

# si necesitamos un separador que no sea un espacio en blanco
paste(text1, text2, sep = "/")

# si no necesitamos separador
paste0(text1, text2)

text <- paste(text1, text2)


# tambien podemnos dividir el texto con la funcion strsplit 
# en palabras
strsplit(x = text, split = " ")

# en oraciones
strsplit(x = text, split = ".", fixed = T)

## modificar mayusculas/minusculas
#todos los caracteres minusculas
tolower(text)







##EXPRESIONES REGULARES
#las expresiones regulares son una serie de operadores y reglas que nos permiten 
#volver muy flexibles y eficientes las busquedas y sustituciones de cadenas de texto

text <- "Otra razón por la que mi hijo y yo amamos @telepacifico porque los niños son tratados con respeto. #LaTvPúblicaSeDefiende https://t.co/pOk0JHifmo"
text <- str_conv(text, "UTF-8")
text <- tolower(text)

#si busco cuantas ocurrencias hay "pacifico" esto es lo que pasa
grepl("pacifico", text)

#pero digamos que yo quiero encontar "pacifico" solo si es una palabra aparte
#como se hace?
#puedo agregar un espacio antes y uno despues
grepl(" pacifico ", text)


text1 <- "creo que el pacifico tiene espacio para crecer"
grepl(" pacifico ", text1)

#pero que pasa con estas dos frases?
text2 <- "pacifico una region colombiana"
text3 <- "muchos futbolistas famosos son del pacifico"
grepl(" pacifico ", text2)
grepl(" pacifico ", text3)

#volvamos flexible la busqueda
grepl("( |^)pacifico( |$)", text2, perl = T)
grepl("( |^)pacifico( |$)", text3, perl = T)



#otro ejemplo
text1 <- "colombia es un lindo pais"
text2 <- "que linda la camiseta de la seleccion"
text3 <- "cuantos lindos pueblos tiene colombia"
text4 <- "lindas las cordilleras colombianas"

#quiero poder buscar todas las variaciones del adejetivo lindo independientemente de
#numero y genero
#lo podemos hacer asi
grepl("( |^)lind(o|a|as|os)( |$)", text1, perl = T)

#o mejor aun asi
grepl("( |^)lind(o|a)s?( |$)", text1, perl = T)
grepl("( |^)lind(o|a)s?( |$)", text2, perl = T)
grepl("( |^)lind(o|a)s?( |$)", text3, perl = T)
grepl("( |^)lind(o|a)s?( |$)", text4, perl = T)



#otra aplicacion
text1 <- "tan lindaaaa"

#quiero sostituir todas las variaciones independientemente del numero de 'a'
gsub(pattern = "lind(a)+", replacement = "linda", text1, perl = T)



#una aplicacion mas
text

#como hago para quitar el enlace?
gsub(pattern = "https://t.co/[a-z0-9]+", replacement = "", text, perl = T)


## Operaciones de texto con data.table

articles.flt.dt[, title.pr := str_conv(title, "UTF-8")]
articles.flt.dt[, title.pr := tolower(title.pr)]
View(articles.flt.dt[, list(title, title.pr)])




################
## EJERCICIOS ##
################








library(quanteda)

## PASOS AVANZADOS HACIA LA DTM


## 1) primero organizo los ariculos que me interesan 
news.flt.dt <- articles.flt.dt[thread.site_type == "news" | thread.site_type == "blog"]
news.flt.dt <- news.flt.dt[order(title)]



# 2) es comun que haya articulos iguales copiados entre sitios diferentes y por 
# lo tanto repetidos... que qriterio usamos?

news.flt.dt[, qrank := frank(published, ties.method="random"), 
            by = c("text","thread.site")]

news.flt.dt <- news.flt.dt[qrank == 1]

# 3) suele pasar tambien que los servicios de crawling de articulos rastreen 
# cambios minimos en los articulos, lo que crea duplicados fuzzy

library(stringdist)
news.flt.dt[, count := .N, by = thread.site_full]
news.flt.dt.single <- news.flt.dt[count == 1]
news.flt.dt.multi <- news.flt.dt[count > 1]

sitios <- unique(news.flt.dt.multi$thread.site_full)

s <- sitios[2]

news.flt.dt.multi.tmp <- news.flt.dt.multi[thread.site_full == s]

n <- nrow(news.flt.dt.multi.tmp)
  

text.ref <- news.flt.dt.multi.tmp[i , text]
    
fuzzy.dt <- news.flt.dt.multi.tmp[, list(uuid, text)]

fuzzy.dt[, text.r :=  text.ref]
    
fuzzy.dt[, res := stringsim(text, text.r, method = "osa")]



#quito los articukios que no tengan titulo
news.flt.dt <- news.flt.dt[title != ""]



## 3) ahora creo el CORPUS 
mycorpus <- corpus(news.flt.dt$title)
mycorpus
head(summary(mycorpus))
mycorpus.dt <- as.data.table(summary(mycorpus))


## 4) a) TOKENIZATION (separar las palabras)
## la tokenization se puede aplicar segun varios metodos
# por palabras
mycorpus.wd <- tokens(mycorpus, what = "word") ###vamos a utilizar este para crear la dtm

# por palabras mas rapido pero menos preciso ,en caso de grandes cantidades de datos 
mycorpus.fw <- tokens(mycorpus, what = "fastestword")

# por caracter
mycorpus.ch <- tokens(mycorpus, what = "character")

# por oraciones
mycorpus.sn <- tokens(mycorpus, what = "sentence")

mycorpus[1548]
mycorpus.wd[1548]
mycorpus.fw[1548]
mycorpus.ch[1548]
mycorpus.sn[1548]




## 3) b) NORMALIZACION DE TEXTOS
# en quanteda tokenization y normalizacion de textos se hacen con la misma funcion "tokens"
# vamos removiendo las url
mycorpus_url <- tokens(mycorpus, what = "word", remove_url = T)

mycorpus[7] 
mycorpus_url[7]

# vamos removiendo urls y numeros
mycorpus_url_numb <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T)

mycorpus[7]
mycorpus_url[7]
mycorpus_url_numb[7]

# vamos removiendo urls, numeros y puntuacion
mycorpus_url_numb_punct <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T,
                                  remove_punct = T)
mycorpus[7]
mycorpus_url_numb[7]
mycorpus_url_numb_punct[7]


# vamos removiendo urls, numeros, puntuacion y otros simbolos, separadores (espacios blancos extra, 
# new line, tab, etc), simbolos de twitter (@#).
mycorpus_url_numb_punct_symb <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T,
                                       remove_punct = T, remove_symbols = T, remove_separators = T, 
                                       remove_twitter = T, remove_hyphens = T)
mycorpus[7]
mycorpus_url_numb_punct[7]
mycorpus_url_numb_punct_symb[7]



## 4)STOPWORDS
## vamos removiendo las stopwords
# estas son las stopwords basicas del espanol
stopwords.es <- stopwords(language = "es")
stopwords.es

# podemos anadir otras stopwords ad hoc
# 1) palabras relacionadas con el tema investigado
# por ejemplo podemos quitar la palabra clave de la busqueda de los articulos
stopwords.add <- "estadistica"

#creo el listado de stop
stopwords.tot <- c(stopwords.es, stopwords.add)
mycorpus_sw <- tokens_remove(mycorpus_url_numb_punct_symb, stopwords.tot)
mycorpus[5]
mycorpus_url_numb_punct_symb[5]
mycorpus_sw[5]


##### CREACION DE LA DOCUMENT TERM MATRIX
#aqui ya podemos crear la document term matrix
dtm_sw <- dfm(mycorpus_sw, tolower = T)
View(dtm_sw[c(1:100), c(1:50)])



##PASOS AVANZADOS
# en caso de que deba efectuar operaciones de machine learning sobre utilizando la dtm lo que
# debo hacer es poder limitar la complejidad de la matriz
# por experimentos miramos como cambia la dtm si la creamos en correspondencia de cada uno de
# las operaciones anteriores
# dtm con una simple tokenization
dtm.wd <- dfm(mycorpus.wd)
dtm.wd
# dtm removiendo las urls
dtm_url <- dfm(mycorpus_url)
dtm_url
# dtm removiendo las urls y numeros
dtm_url_numb <- dfm(mycorpus_url_numb)
dtm_url_numb
# dtm removiendo urls, numeros y puntuacion
dtm_url_numb_punct <- dfm(mycorpus_url_numb_punct)
dtm_url_numb_punct
# dtm removiendo urls, numeros, puntuacion y simbolos
dtm_url_numb_punct_symb <- dfm(mycorpus_url_numb_punct_symb)
dtm_url_numb_punct_symb
# dtm removiendo urls, numeros, puntuacion, simbolos y stopwords
dtm_sw <- dfm(mycorpus_sw)
dtm_sw

#debo reducir mucho mas la complejidad

## 5) STEMMING
# encuentro las raices de las palabras
mycorpus_stem<-tokens_wordstem(mycorpus_sw, language = "es")
mycorpus[10]
mycorpus_sw[10]
mycorpus_stem[10]

dtm_stem <- dfm(mycorpus_stem)
dtm_stem


##6) para disminuir la complejidad podemos quitar los stems de unos o dos caracteres 
dtm_w2<-dfm_select(dtm_stem, min_nchar = 3)
dtm_w2


##remuevo stems no frecuentes, en este caso que aparezcan en menos de 10 titulos 
#y por lo menos en en cinco titulos distintos
dtm_f10<-dfm_trim(dtm_w2, min_termfreq = 10, min_docfreq = 3)
dtm_f10

##7) ahora removemos los articulos que no tienen informacion
# calculamos el numero de palabras por cada titulo
rowTotals <- apply(X = dtm_f10, MARGIN = 1, FUN = sum) 
#eliminamos todos los tweets que no tengan stems en las columnas
dtm_delrow <- dtm_f10[rowTotals> 0, ]
dtm_delrow


# resumiendo
# 1) selecciono la dtm quitando los retweets
# 2) creo el corpus
# 3) tokenization con normalizacion de textos: quitamos urls, numeros, puntacion, simbolos, 
#   separadores, etc.
# 4) removemos las stopwords

# si la creacion de la document sirve para hacer machine learning
# 5) se hace el stemming
# 6) se reduce la cantidad de columnas quedandose con las palabras mas frecuentes
# 7) se quitan las filas que non tienen ninguna informacion








## exploramos la document term matrix
#las palabras mas frecuentes
topfeatures(dtm_sw, 20)


## similarity
# es algo parecido a una correlacion calculada sobre palabras
# que tanto quedan asociadas las palabras entre ellas?
# tomo las diez palabras mas frecuentes
tf <- topfeatures(dtm_sw, 10)
tf
# me quedo con los nombres de las palabras mas frecuentes
freqfeatures <- names(tf)
freqfeatures
# individuo posibles palabras vacias extras dentro de las frecuentes
stopfeatures <- c("vs")
# las quito de listado de las palabras frecuentes
freqfeatures <- freqfeatures[!freqfeatures %in% stopfeatures]

#calculo la matriz de correlaciones entre las palabras mas frecuentes y todas las demas palabras 
sim <- textstat_simil(dtm_sw, selection = freqfeatures, margin = "features", 
                      method = "correlation")
#visualizo solo las correlaciones entre frecuentes
sim[freqfeatures, ]

# como sacar el grafico de las correlaciones
library(GGally)ggcorr(data = NULL, cor_matrix = simtop, nbreaks=10, palette='RdBu', label=TRUE, label_size=4, 
       label_color='black', label_round = 2)



################
## EJERCICIOS ##
################




## N-GRAMS
# los ngrmas son secuencias de n palabaras dentro del texto
# para hacer una dtm con ngramas se siguen los siguentes pasos
# creo un corpus de bigrams o sea todas las secuencias de dos palabras contiguas
mycorpus_2gram <- tokens_ngrams(mycorpus_sw, n = 2, concatenator = " ")
mycorpus_2gram[2]
# creo la dtm sobre esto
dtm_2gram <- dfm(mycorpus_2gram)
dtm_2gram
View(dtm_2gram[c(1:10),c(1:20)])
# encuentro los bigrams mas frecuentes
topfeatures(dtm_2gram, 10)

# vamos con los trigrams
mycorpus_3gram <- tokens_ngrams(mycorpus_sw, n = 3, concatenator = " ")
mycorpus_3gram[2]
dtm_3gram <- dfm(mycorpus_3gram)
View(dtm_3gram[c(1:10),c(1:20)])
# encuentro los bigrams mas frecuentes
topfeatures(dtm_3gram, 10)

# vamos con 1-grams 2-grams 3-grams
mycorpus_ngram <- tokens_ngrams(mycorpus_sw, n = c(1:3), concatenator = " ")
mycorpus_ngram[2]


### WORDCLOUD
# wordcloud
library(RColorBrewer)

textplot_wordcloud(dtm_sw, min_count = 20, random_order = FALSE,
                   rotation = .25, min_size = 1, max_size = 5,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


################
## EJERCICIOS ##
################


library(udpipe)   
## First step: Take the Spanish udpipe model and annotate the text. 
## Note: this takes about 3 minutes   
ud_model <- udpipe_download_model(language = "spanish")   
ud_model <- udpipe_load_model(ud_model$file_model)   
x <- udpipe_annotate(ud_model, x = news.flt.dt$title) %>%
    as.data.table()

sustantivos <- x[upos == "NOUN"]
stats <- txt_freq(x = sustantivos$lemma)


#Co ocurrencia. que tan  frecuentemente las palabras ocurren en la misma sentencia, 
#en este caso sustantivos y adjetivos
sustantivosadjetivos <- x[upos == "NOUN" | upos == "ADJ"]
stats <- cooccurrence(sustantivosadjetivos,    
                      term = "lemma", 
                      group = c("doc_id", "paragraph_id", "sentence_id"))  



################
## EJERCICIOS ##
################


#metricas social
#interacciones


################
## EJERCICIOS ##
################


#actualizar el conteo social
library(httr)

url<-"https://www.r-project.org/"
key<-"a1cba1044f64e002f3eefcba937ac2c6e8cb6682"


response <- GET()

http <- paste0("https://api.sharedcount.com/v1.0/?url=", url, "&apikey=", key)

response <- GET(http)

socialcount <- content(response)

socialcount$Facebook$total_count


#########
#EJERCICIO
#########


#####SENTIMIENTO#####
#####TOPICS#####


## categorias mas frecuentes
# la columna de las categorias en el data.table tiene una estructura particular 
# y hay que tratarla de forma espcifica
# igual por medio de una dtm podemos calcular facilmente los hashtags frecuentes
View(articles.flt.dt)
# empezamos quedandonos con las solas filas que contienen categorias
ht <- articles.flt.dt[is.na(thread.site_categories)==F, "thread.site_categories"]
# la idea es poder collapsar la lista en una cadena de texto separada por espacios
ht[, thread.site_categories_string := lapply(thread.site_categories, 
                                             paste, collapse = " ")]
ht[, list(thread.site_categories, thread.site_categories_string)]

# ahora si creo el corpus de los hashtgas
ht.corpus<-corpus(ht$thread.site_categories_string)
# la dtm
ht.dtm<-dfm(ht.corpus)
# los 15 ht mas frecuentes
topht <- topfeatures(ht.dtm, 15)
# este tipo de vector named no lo puedo tranformar directamente en data.table
# debo pasar por un data.frame
topht <- as.data.frame(topht)
# y ahora si en un data table recordandome de mantener los rownames
topht <- as.data.table(topht, keep.rownames = T)
topht


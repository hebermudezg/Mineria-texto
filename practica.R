install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("tm")
install.packages("SnowballC")
install.packages("caTools")
install.packages("caret")
install.packages("e1071")
install.packages("plyr")
install.packages("wordcloud")

library(twitteR)
library(ROAuth)
library(httr)
library(tm)
library(SnowballC)
library(caTools)
library(caret)
library(e1071)
library(plyr)
library(wordcloud)


library(twitteR)
library(dplyr)
library(tidyr)

consumerKey = 'RJ0dMlb9LSrneRmHCZx5Z6XBU'
consumerSecret = 'wt9Ep1JgyV9PrRdJMoOsOrqsI9QYbR6mgBnJZiCAH8wLhD1rwU' 
accessToken = '1085684706572685312-hMSRrDShD8GI2k61nh5YxDV2WIMgPy'
accessTokenSecret = 'uNWRj1UTztLT4EDHPpGvti2n2xGMMRNAZxDQIPjBhfRJ9' 
setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessTokenSecret)

# encontrar twitters
searchTwitter("seleccion colombia", n = 10, lang = "es")



#tweets <- read.csv("nombre del archivo.csv", sep =";") #cargar los datos al tweets ("nombre del archivo.csv", separador)
load(file = "textominado.RData")


table(tweets$sentiment) #Cuantos twets positivos y negativos hay
Corpus - Corpus(VectorSource(tweets$text)) #objeto corpus se le asigna el objeto de tweets // lee los tweets
length(Corpus) #cuenta las palabras tiene el corpus
content(Corpus[[20]]) #imprime la posicion 20

#Preposesamiento

Corpus <- tm_map(Corpus, tolower) #procesamiento cambia las palabras a minusculas
Corpus <- tm_map(Corpus, PlainTextDocument) #vuelve el corpus normal para poder visualizarlo
content(Corpus[[20]]) #imprime la posicion 20
Corpus <- tm_map(Corpus, removePunctuation) #quita la puntuacion
content(Corpus[[20]]) #imprime la posicion 20
stopwords("spanish")[1:20] #preposiciones etc... [1:20] imprime las 20 primeras / stopwords("english")
Corpus <- tm_map(Corpus, removeWords, c(stopwords("spanish"), "las")) #remueve las stopwords y otras palabras que se deseen
content(Corpus[[20]]) #imprime la posicion 20
Corpus <- tm_map(Corpus, stemDocument)  #smemming devolver las apalabras a su rair "devolvieron" "devolver"
content(Corpus[[20]]) #imprime la posicion 20

#clasificación

frequencies <- DocumentTermMatrix(Corpus)  #crear matriz
frequencies #imprime atributos de frequencies
inspect(frequencies[15:20, 5:10]) #zoop de la matriz / posiciones [15:20, 5:10]
findFreqTerms(frequencies, lowfreq = 50) #frecuencias de palabras iguales o mayoes a 50
sparse <- removeSparseTerms(frequencies, 0.995) #quitar palabras que son mencionadas muy poco / deja las palabras mas usadas
sparse #imprime sparse
tweetsSparse <- as.data.frame(as.matrix(sparse)) # retornar la variable sparse como una base de datos en formato R
colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) #asinar los nombres de cada palabra al dataframe
tweetsSparse$sentiment <- tweets$sentiment #examina con la base de datos de sentimientos precargada

#modelo de clasificación 
#Objetivo: Encontrar  un hiperplano h de dimension (n-1) que separe los ejemplos etiquetados con -1 de los etiquetados con +1 
#con un "margen maximo" (P).
#los SVM buscan los puntos mas cercanos entre varias clases. Estos puntos se llaman "los vectores de soporte"
#SVM luego declara que la mejor linea de separación va a ser la linea que divide las dos clases y que al mismo tiempo 
#maximiza la distancia del hiperplano a los vectores de soporte (r)
#Ecuación del hiperplano: WX+b=0
#Problema de optimización: Encuentre W y b  tal que  P=2/[[W]]

#partir la base de datos en entrenamiento y evaluación 
#(entrenamiento: enseñarle a la BD como funcionar, evaluación: evaluar como lo hace el modelo y su poder de predicción)

set.seed(12)
split <- sample.split(tweetsSparse$sentiment, SplitRatio = 0.8) #decide que observaciones se van para un lado u otro
#plitRatio= 0.8 / el 805 se va a entrenamiento
trainSparse = subset(tweetsSparse, split==TRUE) #particion
TestSparse = subset(tweetsSparse, split==FALSE) #particion
table(textSparse$sentiment)

#algoritmo de clasificación
SVM <- svm(as.factor(sentiment)~ ., data=frameSparse)
summary(SMV) #Descrbe el modelo
predictSVM <- predict(SVM, newdata = TestSparse)
confusionMatrix(predictSVM, TestSparse$sentiment) #evaluar el desempeño de los tweets

positive <- subset(tweetsSparse, tweetsSparse$sentiment=1) #crea base de datos de tweets positivos
positive$sentiment <- NULL

positivas <- as.data.frame(colSums(positive)) #generar frecuencias
positivas$words <- row.names(positivas) #crea variable words
colnames(positivas) <- C("freq","word")
#nube de palabras
wordcloud(positivas$word, positivas$freq, random.order = FALSE, colors = brewer.pal.info(8,"Dark2"), max.words = 300)
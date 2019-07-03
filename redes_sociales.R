#install.packages("twitteR")
#install.packages("dplyr")
#install.packages("tidyr")

library(twitteR)
library(dplyr)
library(tidyr)

consumerKey = 'RJ0dMlb9LSrneRmHCZx5Z6XBU'
consumerSecret = 'wt9Ep1JgyV9PrRdJMoOsOrqsI9QYbR6mgBnJZiCAH8wLhD1rwU' 
accessToken = '1085684706572685312-hMSRrDShD8GI2k61nh5YxDV2WIMgPy'
accessTokenSecret = 'uNWRj1UTztLT4EDHPpGvti2n2xGMMRNAZxDQIPjBhfRJ9' 
setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessTokenSecret)
#============================================================================

# encontrar twitters
#a <- searchTwitter("ciencia de datos", n = 500, lang = "es")
#tweetsdf<- twListToDF(a)
#write.csv(tweetsdf,file = "bases/tweets.csv", row.names = F)
tweets<-read.csv("bases/tweets.csv")

#User timeline
t<- getUser('BharatendraRai')
t
userTimeline(t,n=3)
#============================================================================
#tendencias
trend<-availableTrendLocations()
head(trend)
world <- getTrends(1)
medellin<- getTrends(368150)
bogota<-getTrends(368148)
#============================================================================
#Corpus
corpus<-iconv(tweets$text, to = "utf-8-mac")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
#============================================================================
#Clean text
corpus <- tm_map(corpus,tolower)#minusculas
corpus <- tm_map(corpus,removePunctuation)#signos de puntuntuacion
corpus <- tm_map(corpus,removeNumbers)#Numeros
cleanset <- tm_map(corpus,removeWords,stopwords("es"))#Numeros
removeURL<-function(x) gsub('https[[:alnum:]]*','',x)
cleanset<- tm_map(cleanset,content_transformer(removeURL))
cleanset<- tm_map(cleanset, stripWhitespace)

inspect(corpus[1:5])
#============================================================================
#term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm<- as.matrix(tdm)
tdm[1:10,1:20]
#============================================================================
#Bar plot (frecuencia de palabras)
w<- rowSums(tdm)
w<- subset(w, w>= 15)
barplot(w,las = 2, col = rainbow(50))
#============================================================================
#Word cloud
library(wordcloud)
set.seed(222)
w<-sort(rowSums(tdm),decreasing = T)
win.graph()
wordcloud(words = names(w),freq = w, max.words = 200,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, "Dark2"),
          scale = c(7,0.3))
# Nube muy bonita 
library(wordcloud2)
w<- data.frame(names(w),w)
colnames(w)<-c('word','freq')
wordcloud2(w, size=0.8,shape='circle')
# Nube en forma de estrella
wordcloud2(w, size=0.8,shape='star',
           rotateRatio = 0.5,
           minSize = 1)
# Nube en forma de triangulo
wordcloud2(w, size=0.7,shape='triangle',
           rotateRatio = 0.5,
           minSize = 1)
#Nube en forma de A
letterCloud(w,  word = "apple", size = 2)
#============================================================================
#ANALISIS DE SENTIMIENTOS
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
nrc_es<- read.csv("bases/nrc_es.csv")
colnames(nrc_es)<-c("word","sentiment")
#file
Tweets<-iconv(tweets$text, to = 'utf-8-mac')
#obtener sentimientos lexico nrc
s<- get_nrc_sentiment(Tweets, language = "spanish")
#barplot
barplot(colSums(s),las=2, col = rainbow(10),
        ylab = "Conteo",
        main =  "Sentimentos score ofertas laborales twitter")







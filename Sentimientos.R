#===============================================================================
#===================Leer y limpiar las bases de datos===========================
#===============================================================================
#comprender la opini??n o emoci??n en el texto

library(tidyverse)      # data manipulation & plotting
library(tidytext)       # provides additional text mining functions
library(stringr)        # text cleaning and regular expressions
library(forcats)
library(dplyr)
library(tm)

# Cargar terminologias
load(file = "bases/textominado.RData")
tweets<-read.csv("bases/tweets.csv")

# Creando marco de datos. 

titles <- c("tweets ofertas ciencia de datos")
tweets_ofertas<- as.character(tweets$text)
books <- list(tweets_ofertas)

## Tokenizar 

series <- tibble()
for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    dplyr::select(book, everything())
  
  series <- rbind(series, clean)
}

# convirtiendo a factor los documentos(terminos)
series$book <- factor(series$book, levels = rev(titles))
series
class(series)

#### Definimos las stop words en espa??ol################################################
stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "as??", "hacer", "si",
                                    "uso", "debe", "tipo", "a??os", "pueden", "puede",
                                    "si", "s??", "NA", "NA NA",NA,"requiere",
                                    "oportunidades","aqui", "ofertas",
                                    "horas", "importante","nuevo","id",
                                    "sector","trabajo","personal","salario",
                                    "nuevos","dos","requisition","id","contrato",
                                    "a??os","ai","1", "2018","2","2026",
                                    "u","00f3","rt","https","t.co","00ed",
                                    "amio","n","00e1","ene","n"))
########################################################################################

## contando palabras mas comunes en todo el texto de la serie CON FILTRO
series %>%
  count(word, sort = TRUE)
# contando palabras mas comunes en todo el texto de la serie, pero agrupados por 
# terminos y CON FILTRO
series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras)  %>% 
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>% 
  # visualizacion de la frecuencia absoluta de palabras por terminos consultados
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "T??rminos", y = "Frecuencia") +
  coord_flip() +
  theme(legend.position="none")
#===============================================================================
#==========================Lexico - sentimientos en espaniol====================
#===============================================================================
#El tidytextpaquete contiene tres l??xicos de sentimiento:
#AFINN - puntuacion de -3 a 2
#bing - negativo o positivo
#nrc - sentimiento- triste,miedo,enojo
#basados en unigramas (o palabras individuales) en ingles:
head(sentiments)
#Utilizaremos versiones de estas en espaniol

affin_es<-read.csv("bases/afinn_es.csv",stringsAsFactors = F, fileEncoding = "latin1")%>% 
  tbl_df()
affin_es$Puntuacion<-as.factor(affin_es$Puntuacion)

bing_es<-read.csv("bases/bing_es.csv")
bing_es<-bing_es[,-c(2,4,5)]
colnames(bing_es)<- c("word","sentiment")
bing_es$word<-as.character(bing_es$word)
bing_es$sentiment<- as.character(bing_es$sentiment)


nrc_es<- read.csv("bases/nrc_es.csv")
colnames(nrc_es)<-c("word","sentiment")

#===============================================================================
#=============================Analisis basico de sentimiento====================
#=====================================lexico nrc================================

#Contar palabras con algun setimiento
series %>%
  right_join(nrc_es) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)
# podemos ver c??mo la trama de cada novela cambia hacia un sentimiento m??s 
# positivo o negativo a lo largo de la trayectoria
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(bing_es) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

#===============================================================================
#=======================Palabras comunes de sentimiento=========================
#===============================lenxico bing====================================

bing_word_counts <- series %>%
  inner_join(bing_es) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

#==============================================================================
#======================Con unidades mas grandes NO HACER:SOLO PARA CAPITULOS============================
#===========================Lexico afinn=======================================

# Estos algoritmos tratan de entender que:

# "No estoy teniendo un buen d??a"
# Es una frase triste, no feliz, debido a la negaci??n.

tibble(text = tweets_ofertas) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# Dividir ofertas_ciencia_de_datos por cap??tulo y oraci??n.

ps_sentences <- tibble(chapter = 1:length(tweets_ofertas),
                       text = tweets_ofertas) %>% 
  unnest_tokens(sentence, text, token = "sentences")


book_sent <- ps_sentences %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(desc(sentiment))

book_sent

# Podemos visualizar esto con un mapa de calor que muestra los sentimientos 
# m??s positivos y negativos a medida que avanzamos en cada cap??tulo

ggplot(book_sent, aes(index, factor(chapter, levels = sort(unique(chapter), decreasing = TRUE)), fill = sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Chapter Progression", y = "Chapter") +
  ggtitle("Sentimiento de Ofertas en ciencia de datos",
          subtitle = "Summary of the net sentiment score as you progress through each chapter") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

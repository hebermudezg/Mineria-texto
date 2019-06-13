text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text



# En marco de datos ----
# coloquemos este texto en un marco de datos.
library(dplyr)
text_df <- tibble(line = 1:4, text = text)
text_df

class(text_df)

# Tokenizar-----
#necesitamos dividir el texto en tokens individuales
#install.packages("tidytext")
library(tidytext)
text_df %>% unnest_tokens(word, text)

# 1. elimina la puntuacion 
# 2. convierte en minusculas.
#to_lower = FALSEargumento 


# eliminar stop words---------------------
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%ungroup()

# Tokenizar
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)


tidy_books %>%
  count(word, sort = TRUE)

#plot(tidy_books)



### visualización. ----

# Contar frecuencias de las palabras. 
library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

############ -------------------------------------

library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# tokenizndo por palabras y quitandole la stop-words
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# contando la frecuencia de las palabras
tidy_hgwells %>%
  count(word, sort = TRUE)


bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)



library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

# Graficando -----------


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


# Las palabras que están cerca de la línea en estas gráficas tienen frecuencias similares en ambos conjuntos de textos,


#------------------ prueba de correlacion ------------------------------------

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)







### sentimientos -----------------------------------------


library(tidytext)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

##¿Cómo se juntaron y validaron estos léxicos de sentimientos?
# Fueron construidos a través de crowdsourcing (utilizando, por ejemplo, Amazon Mechanical Turk)




library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

library(tidyr)
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


library(ggplot2)
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



################## Tokenizar por oraciones-------------------------
#######################################################


PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
PandP_sentences$sentence[1]




########################### Analisis de frecuencias 

# término frecuencia (tf)
# frecuencia de documentos inversos (idf)







############################ n-grama ########################

library(dplyr)

library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
class(austen_books())

austen_bigrams


# contando y filtrando n gramas

austen_bigrams %>%
  count(bigram, sort = TRUE)
# muchas palabras no interesnates comunes como in to of


# separando las palabras en dos columnas --- 

library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# volviendo a unir las palabras pero sin cotar con la palabras comunes

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united


################# 3 gramas ----------------------

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)



#  Analizando bigramas


bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)





###########################  Red de bi-gramas #############################
library(igraph)

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# filtro solo para combinaciones relativamente comunes

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph



#### Graficos palabras bi-gramas que ocurrieron mas de 20 veces. 

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



#Contando y correlacionando pares de palabras con el paquete widyr





########################################################
#######################################################3
#########################################################


library(tm)   # específico para minería de textos.
library(SnowballC)
library(wordcloud) #para graficar nubes de palabras
library(ggplot2)
library(dplyr) # manipulacion de datos
library(readr)
library(cluster)

# trabajemos con el texto niebla.
nov_raw <- read_lines("49836-0.txt", skip = 419, n_max = 8313-419)
str(nov_raw) # tipo caracter. 
length(nov_raw)


nov_text <- cbind(
    rep(1:ceiling(length(nov_raw)/10), each = 10) %>%
      .[1:length(nov_raw)],
    nov_raw
  ) %>%
  data.frame %>%
  aggregate(
    nov_raw ~ V1,
    data = .,
    FUN = paste,
    collapse=" ") %>%
  select(nov_raw) %>%
  as.matrix

dim(nov_text)

nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
  


## removiendo cosas -----------------------------------
nov_text <- removeWords(nov_text, words = stopwords("spanish")) # removiendo stop words
nov_text <- removePunctuation(nov_text) # removiendo puntuaciones
nov_text <- removeNumbers(nov_text)     # removiendo numeros 
nov_text <- stripWhitespace(nov_text)   #removiedo espacios en blanco

# creando copus. 
# acervo de documento a analizar.----------------------
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus

stop_words
stopwords("spanish")


# nuve de palabras ----------------------------
#nov_ptd <- tm_map(nov_corpus, PlainTextDocument)
wordcloud(nov_corpus, max.words = 80, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))






# removiendo mas palabras no importantes --------------------------------
nov_text <- removeWords(nov_text, words = c("usted", "pues", "tal", "tan", "así", "dijo", "cómo", "sino", "entonces", "aunque", "don", "doña"))
# nuevo copus sin palabras importantes
wordcloud(nov_text, max.words = 80, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
#nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)

wordcloud(
  nov_corpus, max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)


#Term Document Matrix

nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm


# Frecuencia de palabras
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)


nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)


##### graficas de frecuencias

nov_mat[1:10, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes en Niebla",  x = "Palabras", y = "Número de usos")





###### Asociaciones entre palabras

findAssocs(nov_tdm, terms = c("augusto", "eugenia", "hombre", "mujer"), corlimit = .25)



########### Agrupamiento jerarquico ## 
nov_new <- removeSparseTerms(nov_tdm, sparse = .95)
nov_new <- nov_new %>% as.matrix()
nov_new <- nov_new / rowSums(nov_new)
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")








########################################################## HARRY ##############################
############################################################################################

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")


# librerias 

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)    # provides the first seven novels of the Harry Potter series


philosop
## Texto ordenando --------------------------------------

# para analizar correctamente este texto queremos convertirlo en un marco de datos o tibble.
philosophers_stone[1:2]
text_tb <- tibble(chapter = seq_along(philosophers_stone),
                  text = philosophers_stone)


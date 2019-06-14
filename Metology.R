
######## para un ejemplo #############
hh <- as.matrix(ofertas_empleo[1])
removeWords(hh,stopwords("spanish"))
removePunctuation(hh)
removeNumbers(hh)
stripWhitespace(hh)



## Metodologia ##

# 1. Recoleccion de la informacion
# 2. Formato ordenado tibble
# 3. tokenizacion
# 4. analsis
#  - frecuencias de palabras
#  - eliminar stopwords
#  - vizualizacion
# 5. 

######################################################
################ Analisis con todos los datos ########
######################################################


load(file = "textominado.RData")


library(dplyr)
library(tidyverse)

library(tidytext) #
library(tm) #
library(wordcloud)#


# creando marco de datos

titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)




## limpiemos los datos antes
series <- tibble()
for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}




# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series
class(series)






## contando frecuencias ------------------------

series %>%
  count(word, sort = TRUE)



## eliminado stopwords -------------------------------
## haciendo nube de palabras

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA", "1", "2018", "inglés",
                                    "the", "cómo", "dos"))

# quitnado stopwors y mas palabras
series <- series %>% anti_join(stop_words_spanish) 
series <- series %>% anti_join(mas_palabras) 






# nube de palabras de 1-grama
#win.graph()
series %>%
  count(word, sort = TRUE) %>% with(wordcloud(unique(word), n, max.words =60,
                                              random.order = F,colors = brewer.pal(name = "Dark2", n = 8)))





# conteo agrupando por término

series %>%group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) 





####### palabra individual
# Podemos visualizar esto con
# visualizacion de la frecuencia absoluta de palabras por términos consultados

series %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, n), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "", y = "Frecuencia", title = "Frecuencia de palabras") +
  coord_flip() + theme(legend.position="none")


########################################
################ bi-grama  #############


series <- tibble()
for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# convertir titulos a factores
series$book <- factor(series$book, levels = rev(titles))
series



## eliminado stopwords -------------------------------
## haciendo nube de palabras

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA", "1", "2018", "inglés",
                                    "the", "cómo", "dos", "en", "el", "la", "na"))

# quitnado stopwors y mas palabras
series <- series %>% anti_join(stop_words_spanish) 
series <- series %>% anti_join(mas_palabras) 


series %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  filter(!word1 %in% mas_palabras$word,
         !word2 %in% mas_palabras$word) %>%
  count(book,word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(8) %>%
  ungroup() %>% filter(bigram != "NA NA") %>% 
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(reorder(bigram,n), n, fill = book))+
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE)+
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()









##########################################################
################## Nubes por categoria ##################
########################################################


series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "ofertas de empleo") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "big data") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "estadística") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 


series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "machine learning") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 


# calculemos la frecuencia de cada palabra en toda el conjuto de datos
# y lo comparamos con cada terminos


# calculate percent of word use across all novels
potter_pct <- series %>%
  anti_join(stop_words_spanish) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each novel
frequency <- series %>%
  anti_join(stop_words_spanish) %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()

frequency

library(ggplot2)
# visualizacion 
win.graph()
ggplot(frequency, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "tomato", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Términos consultados", x = NULL)




######### correlaciones 

frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)



###########################################
##################### sentimientos

library(tm)
help(tm)










######################################################
############################# n-gram analisis #######
#####################################################
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)
library(forcats)


series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

series



series %>%
  count(bigram, sort = TRUE)


series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)



library(tm)
library(forcats)

stop_words_spanish <- data.frame(word = stopwords("spanish"))
series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  count(book, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(drlib::reorder_within(bigram, n, book), n, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  drlib::scale_x_reordered() +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()

######## para un ejemplo #############
hh <- as.matrix(ofertas_empleo[1])
removeWords(hh,stopwords("spanish"))
removePunctuation(hh)
removeNumbers(hh)
stripWhitespace(hh)



## Metodologia ##

# 1. Recoleccion de la informacion
# 2. Formato ordenado tibble
# 3. tokenizacion
# 4. analsis
#  - frecuencias de palabras
#  - eliminar stopwords
#  - vizualizacion
# 5. 

######################################################
################ Analisis con todos los datos ########
######################################################


load(file = "textominado.RData")


library(dplyr)
library(tidyverse)

library(tidytext) #
library(tm) #
library(wordcloud)#


# creando marco de datos

titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)




## limpiemos los datos antes
series <- tibble()
for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}




# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series
class(series)






## contando frecuencias ------------------------

series %>%
  count(word, sort = TRUE)



## eliminado stopwords -------------------------------
## haciendo nube de palabras

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA", "1", "2018", "inglés",
                                    "the", "cómo", "dos"))

# quitnado stopwors y mas palabras
series <- series %>% anti_join(stop_words_spanish) 
series <- series %>% anti_join(mas_palabras) 






# nube de palabras de 1-grama
#win.graph()
series %>%
  count(word, sort = TRUE) %>% with(wordcloud(unique(word), n, max.words =60,
                                              random.order = F,colors = brewer.pal(name = "Dark2", n = 8)))





# conteo agrupando por término

series %>%group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) 





####### palabra individual
# Podemos visualizar esto con
# visualizacion de la frecuencia absoluta de palabras por términos consultados

series %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, n), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "", y = "Frecuencia", title = "Frecuencia de palabras") +
  coord_flip() + theme(legend.position="none")


########################################
################ bi-grama  #############


series <- tibble()
for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# convertir titulos a factores
series$book <- factor(series$book, levels = rev(titles))
series



## eliminado stopwords -------------------------------
## haciendo nube de palabras

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA", "1", "2018", "inglés",
                                    "the", "cómo", "dos", "en", "el", "la", "na"))

# quitnado stopwors y mas palabras
series <- series %>% anti_join(stop_words_spanish) 
series <- series %>% anti_join(mas_palabras) 


series %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  filter(!word1 %in% mas_palabras$word,
         !word2 %in% mas_palabras$word) %>%
  count(book,word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(8) %>%
  ungroup() %>% filter(bigram != "NA NA") %>% 
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(reorder(bigram,n), n, fill = book))+
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE)+
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()









##########################################################
################## Nubes por categoria ##################
########################################################


series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "ofertas de empleo") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "big data") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "estadística") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 


series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "machine learning") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 


# calculemos la frecuencia de cada palabra en toda el conjuto de datos
# y lo comparamos con cada terminos


# calculate percent of word use across all novels
potter_pct <- series %>%
  anti_join(stop_words_spanish) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each novel
frequency <- series %>%
  anti_join(stop_words_spanish) %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()

frequency

library(ggplot2)
# visualizacion 
win.graph()
ggplot(frequency, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "tomato", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Términos consultados", x = NULL)




######### correlaciones 

frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)



###########################################
##################### sentimientos

library(tm)
help(tm)










######################################################
############################# n-gram analisis #######
#####################################################
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)
library(forcats)


series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

series



series %>%
  count(bigram, sort = TRUE)


series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)



library(tm)
library(forcats)

stop_words_spanish <- data.frame(word = stopwords("spanish"))
series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  count(book, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(drlib::reorder_within(bigram, n, book), n, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  drlib::scale_x_reordered() +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()


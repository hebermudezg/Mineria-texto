
## Metodologia ##

# 1. Recoleccion de la informacion
# 2. Formato ordenado tibble
# 3. tokenizacion
# 4. analsis
#  - frecuencias de palabras
#  - eliminar stopwords
#  - vizualizacion
# 5. 


##### 1 ----
load(file = "textominado.RData")



# 2. Formato odenado tibble ------------------------
text_tb <- tibble(chapter = seq_along(texto), text = texto)



# 3. Tokenizacion ------------------------- por 1-grama - bigrama
# tokenizacion por palabra
tokenizado <- text_tb %>% unnest_tokens(word, text)


# n-grama
bigrams <- text_tb %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams

# NOTA: unnest_tokens
# 1. divide el texto en palabras simpes
# 2. quita los signos de puntuación.
# 3 convierte en minusculas  
# to_lower = FALSEargumento para desactivarlo



## Frecuencia de palabra  ----------------------------------------
tokenizado %>% count(word, sort = TRUE)



## eliminado palbras stopwords -----------------------------------
stop_words_spanish <- data.frame(word = stopwords("spanish"))
tokenizado_ordenado <- tokenizado %>% anti_join(stop_words_spanish ) %>%
  count(word, sort = TRUE)



## vizualizacion --------------------------------------------
# vizualizando freciencias
ggplot(data = tokenizado_ordenado[1:15,], aes(x=word, y=n)) +
  geom_bar(stat = "identity",  fill=rainbow(15)) + coord_flip()



######################################################
################ Analisis con todos los datos ########
######################################################

library(dplyr)
library(tidytext) 
library(tidyverse)
library(tm)
library(wordcloud)

load(file = "textominado.RData")


titles <- c("ofertas de empleo", "machine learning",
             "ciencia de datos", "estadística",
             "inteligencia artificial", "big data")

books <- list(ofertas_empleo, machine_learning,
                    ciencia_de_datos, estadistica,
                    inteligencia_artificial, big_data)

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


## contando frecuencias ------------------------

series %>%
  count(word, sort = TRUE)



## eliminado stopwords -------------------------------
## haciendo nube de palabras

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA"))

#win.graph()
series %>%
  anti_join(stop_words_spanish) %>% anti_join(mas_palabras) %>%
  count(word, sort = TRUE) %>% with(wordcloud(unique(word), n, max.words = 50,
                                              random.order = F,
                                              colors = brewer.pal(name = "Dark2", n = 8)))




  series %>%
  anti_join(stop_words_spanish) %>% anti_join(mas_palabras) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) 





####### palabra individual
# Podemos visualizar esto con
# visualizacion de la frecuencia absoluta de palabras por términos consultados

series %>%
  anti_join(stop_words_spanish) %>% anti_join(mas_palabras) %>%
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


stop_words_spanish$word <- as.character(stop_words_spanish$word)
mas_palabras$word <- as.character(mas_palabras$word)

series %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  filter(!word1 %in% mas_palabras$word,
         !word2 %in% mas_palabras$word) %>%
  count(book,word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup() %>%
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

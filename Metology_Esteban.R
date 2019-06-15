
################################################################################
##################################### GRAFOS ###################################
################################################################################


#---------------------------- datos y librerias -----------------------------
#----------------------------------------------------------------------------

load(file = "textominado.RData")
library(tidyverse)
library(dplyr)#
library(tidytext) #
library(tm) #
library(wordcloud) #
library(ggplot2) #



#------------------------- marco de datos con bi-gramas ------------------------
#-------------------------------------------------------------------------------

titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)

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



############ los bigramas mas comunes #################

series %>%
  count(bigram, sort = TRUE)




##------------------------eliminado stopwords --------------------------------------------
#-----------------------------------------------------------------------------------------


stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", NA, "1", "2018", "inglés",
                                    "the", "cómo", "dos", "bus003507", "bus003507"))

# quitnado stopwors y mas palabras

series <- series %>%
  separate(bigram, c("word1", "word2"), sep = " ")



series <- series %>%  filter(!word1 %in% stop_words_spanish$word  & !word1 %in% mas_palabras$word,
         !word2 %in% stop_words_spanish$word & !word1 %in% mas_palabras$word)


series %>%   count(book, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(bigram, n, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()






#############  Visualización de redes n-gram  ######################

library(igraph)
bigram_graph <- series %>% count(word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  filter(n > 20) %>%  graph_from_data_frame()
bigram_graph



library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()




###################################################
############# Correlación de palabras  ############
###################################################


# calculate percent of word use across all novels
potter_pct <- series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each novel
frequency <- series %>%
  anti_join(stop_words) %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()

frequency










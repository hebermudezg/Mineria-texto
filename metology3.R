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
# 2. quita los signos de puntuaci??n.
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

library(forcats)
library(devtools)
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(dplyr)
library(tm)
library(wordcloud)

# Cargar terminologias
load(file = "textominado.RData")

# Creando marco de datos. 

titles <- c("ofertas ciencia de datos", "ofertas estad??stica", "machine learning",
            "ciencia de datos", "estad??stica",
            "inteligencia artificial", "big data", "anal??tica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)

## tokenizar

series <- tibble()
for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    dplyr::select(book, everything())
  
  series <- rbind(series, clean)
}

## contando palabras mas comunes en todo el texto de la serie SIN FILTRO
series %>%
  count(word, sort = TRUE)

#### Definimos las stop words en espa??ol################################################
stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "as??", "hacer", "si",
                                    "uso", "debe", "tipo", "a??os", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA",NA,"requiere",
                                    "oportunidades","aqui", "ofertas",
                                    "horas", "importante","nuevo","id",
                                    "sector","trabajo","personal","salario",
                                    "nuevos","dos","requisition","id","contrato",
                                    "años","ai","horario","lunes","vez","parte","in",
                                    "the","of","cualquier"))
########################################################################################

#Hacemos nuestra nube de palabras mas frecuentes, con Filtro
series %>%
  anti_join(stop_words_spanish) %>% anti_join(mas_palabras) %>% 
  count(word, sort = TRUE) %>% with(wordcloud(unique(word), n, max.words = 50,
                                            random.order = F,
                                            colors = brewer.pal(name = "Dark2", n = 8)))

# contando palabras más comunes en todo el texto de la serie, pero agrupados por 
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
  labs(x = "Términos", y = "Frecuencia") +
  coord_flip() +
  theme(legend.position="none")

###########################################################
######################## bi-gramas  #######################
###########################################################

series <- tibble()
for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    dplyr::select(book, everything())
  
  series <- rbind(series, clean)
}

# convertir titulos a factores
series$book <- factor(series$book, levels = rev(titles))

#Vizualizar bigrama con filtros, agrupado por terminologias
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


#Conteo sin filtro de pares de palabras
series %>%
  count(bigram, sort = TRUE)

#Conteo con filtro de pares de palabras
series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word) %>%
  filter(!word1 %in% mas_palabras$word,
         !word2 %in% mas_palabras$word) %>% 
  count(word1, word2, sort = TRUE)



##########################################################
################## Nubes por categoria ##################
########################################################


series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "ofertas de empleo") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "big data") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=50, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 



series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "estad??stica") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 


series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% filter(book == "machine learning") %>%
  with(wordcloud(words = word, freq = n, min.freq = 1,
                 max.words=100, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))) 

###########################################################
################## Analis de Porcentajes ##################
###########################################################

# calculemos la frecuencia de cada palabra en toda el conjuto de datos
# y lo comparamos con cada terminos

# porcentaje de uso de palabras en todas las terminologias
percent_all <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras)  %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))
percent_all

# porcentaje de frecuencia de palabras cada terminologia
frequency <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras)  %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(percent_all) %>%
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
  labs(y = "T??rminos consultados", x = NULL)

######### Graficos de red 

#Para mas informacion sobre las múltiples relaciones 
#que existen entre las palabraspodemos utilizar un gráfico de red
library(igraph)

(bigram_graph <- series %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words_spanish$word,
           !word2 %in% stop_words_spanish$word) %>%
    filter(!word1 %in% mas_palabras$word,
           !word2 %in% mas_palabras$word) %>%
    count(word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    filter(n > 10) %>%
    graph_from_data_frame()
)
#Para visualizar nuestra red, usamos el paquete ggraph  que 
#convierte un objeto igraph en un gráfico de tipo ggplot.
library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#===============================================================================
#es posible que también queramos entender qué pares de palabras 
#aparecen conjuntamente cada terminologia

(ps_words <- tibble(chapter = seq_along(machine_learning),
                    text = machine_learning) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words_spanish$word) %>% 
    filter(!word %in% mas_palabras$word))
#Podemos aprovechar el widyrpaquete para contar pares de palabras 
#comunes que aparecen conjuntamente en el mismo capítulo:
library(widyr)

(word_pairs <- ps_words %>%
    pairwise_count(word, chapter, sort = TRUE))
#===============================================================================
#aqui permite buscar qué palabras siguen con más frecuencia a cierto termino
word_pairs %>% filter(item1 == "machine")
#===============================================================================
#También podemos querer saber con qué frecuencia las palabras aparecen juntas 
#de acuerdo a la correlación entre las palabras

#La pairwise_cor()función en widyr nos permite encontrar la correlación entre 
#las palabras según la frecuencia con la que aparecen en la misma sección. 
#Su sintaxis es similar a la pairwise_count().

(word_cor <- ps_words %>%
    group_by(word) %>%
    filter(n() >= 10) %>%
    pairwise_cor(word, chapter) %>%
    filter(!is.na(correlation)))

###########################################################
######################## Grafos  #######################
###########################################################
    
# determina scuales on las palabras correlacionadas más altas que aparecen 
# con alguna palabra en especial
word_cor %>%
  filter(item1 == "machine") %>%
  arrange(desc(correlation))
#ggraph es para visualizar bigrams, 
#podemos usarlo para visualizar las correlaciones dentro de grupos de palabra
ps_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, chapter) %>%
  filter(!is.na(correlation),
         correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#estadística

word_cor %>%
  filter(item1 == "estadística") %>%
  arrange(desc(correlation))

#vizualizacion
ps_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, chapter) %>%
  filter(!is.na(correlation),
         correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#big

word_cor %>%
  filter(item1 == "big") %>%
  arrange(desc(correlation))

#vizualizacion
ps_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, chapter) %>%
  filter(!is.na(correlation),
         correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#analitica

word_cor %>%
  filter(item1 == "analítica") %>%
  arrange(desc(correlation))

#vizualizacion
ps_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, chapter) %>%
  filter(!is.na(correlation),
         correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


###########################################################
######################## Grafos  #######################
###########################################################

library(igraph)


# Toma una terminologia
(ps_words <- tibble(chapter = seq_along(machine_learning),
                    text = machine_learning) %>% 
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words_spanish$word) %>% 
   filter(!word %in% mas_palabras$word))

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

# Toma una terminologia
(ps_words <- tibble(chapter = seq_along(estadistica),
                    text = estadistica) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words_spanish$word) %>% 
    filter(!word %in% mas_palabras$word))
#correlacion
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

#big data

# Toma una terminologia
(ps_words <- tibble(chapter = seq_along(big_data),
                    text = big_data) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words_spanish$word) %>% 
    filter(!word %in% mas_palabras$word))

#correlacion
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


#analitica de datos
(ps_words <- tibble(chapter = seq_along(Analitica_de_datos),
                    text = Analitica_de_datos) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words_spanish$word) %>% 
    filter(!word %in% mas_palabras$word))


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
      








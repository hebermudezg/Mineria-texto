

#----------------------- Metodologia------------------------------
#----------------------------------------------------------------#

# Un requisito fundamental para hacer analisis de texto es tener el texto ordenado
# a menudo el texto viene en formato no estructurado.


# 1. Recoleccion de la informacion
# 2. Formato ordenado tibble
# 3. tokenizacion
# 4. analsis
#  - frecuencias de palabras
#  - eliminar stopwords
#  - vizualizacion
# 5. 




# 1. recoleccion  ------------------
load(file = "textominado.RData")





# 2. Formato odenado tibble ------------------------
text_tb <- tibble(chapter = seq_along(texto), text = texto)
text_tb
# contiene dos columnas, 1. la enumeracion, 2.el contenido de la vacante



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


## contando frecuencias

series %>%
  count(word, sort = TRUE)




## eliminado stopwords
stop_words_spanish <- data.frame(word = stopwords("spanish"))
series %>%
  anti_join(stop_words_spanish) %>%
  count(word, sort = TRUE)



series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10)


# Podemos visualizar esto con
# visualizacion de la frecuencia absoluta de palabras por términos consultados


series %>%
  anti_join(stop_words_spanish) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")




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


# visualizacion 

ggplot(frequency, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Harry Potter Series", x = NULL)




######### correlaciones 

frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)

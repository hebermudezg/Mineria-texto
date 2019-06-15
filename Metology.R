reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


#' @rdname reorder_within
#' @export
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


#' @rdname reorder_within
#' @export
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}





######## para un ejemplo #############
hh <- as.matrix(ofertas_ciencia_de_datos[1])
removeWords(hh,stopwords("spanish"))
removePunctuation(hh)
removeNumbers(hh)
stripWhitespace(hh)
hh <- tibble(hh)
hh %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)


######################################################
################ Analisis con todos los datos ########
######################################################

library(ggplot2) #
library(dplyr)
library(tidytext) 
library(tidyverse)
library(wordcloud)
library(stringr) 
library(tm)
library(forcats)

# Cargar terminologias
load(file = "textominado.RData")

# Creando marco de datos. 

titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)

## Tokenizar 

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


## contando palabras mas comunes en todo el texto de la serie SIN FILTRO
series %>%
  count(word, sort = TRUE)

#### Definimos las stop words en espa??ol################################################
stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA",NA,"requiere",
                                    "oportunidades","aqui", "ofertas",
                                    "horas", "importante","nuevo","id",
                                    "sector","trabajo","personal","salario",
                                    "nuevos","dos","requisition","id","contrato",
                                    "años","ai","1", "2018", "cómo", "the", "lunes", "viernes"))
########################################################################################

#Hacemos nuestra nube de palabras mas frecuentes, con Filtro
series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>% 
  count(word, sort = TRUE) %>%
  with(wordcloud(unique(word), n, 
                 max.words = 50,
                 random.order = F, colors = brewer.pal(name = "Dark2", n = 8)))




################### grafico 1-grama ###############################

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
  labs(x = "Términos", y = "Frecuencia") +
  coord_flip() +
  theme(legend.position="none")


###########################################################
######################## bi-gramas  #######################
###########################################################

stop_words_spanish <- data.frame(word = stopwords("spanish"))
mas_palabras <- data.frame(word = c("tener", "cada", "ser", "así", "hacer", "si",
                                    "uso", "debe", "tipo", "años", "pueden", "puede",
                                    "si", "sí", "NA", "NA NA",NA,"requiere",
                                    "oportunidades","aqui", "ofertas",
                                    "horas", "importante","nuevo","id",
                                    "sector","trabajo","personal","salario",
                                    "nuevos","dos","requisition","id","contrato",
                                    "años","ai","1", "2018", "cómo", "the", "lunes", "viernes"))




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




series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_spanish$word,
         !word2 %in% stop_words_spanish$word)%>%
  filter(!word1 %in% mas_palabras$word,
         !word2 %in% mas_palabras$word)%>%
    count(book, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(reorder_within(bigram, n, book), n, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()






###########################################################
################## Analisis de Porcentajes ##################
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
  labs(y = "Términos consultados", x = NULL)




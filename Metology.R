#
# Definiendo algunas funciones para graficar adecuadamente. 


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

hacer_nubes <- function(n){
  library(wordcloud)
  n <- wordcloud(n$word, n$n, min.freq = 2, random.order = FALSE,
                 random.color = FALSE, max.words = 500, colors = brewer.pal(8,"Dark2" ))
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

# Cargar terminologias (documentos tecuperados de las busquedas en internet)
load(file = "bases/textominado.RData")



# Creando marco de datos. (formato ordenado tibble y toqenizar)
titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")

books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)

## Tokenizar 

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


## contando palabras mas comunes en todo el texto de la serie SIN FILTRO
nub <- series %>%
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
#win.graph()
nub2 <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>% 
  count(word, sort = TRUE) %>%
  with(wordcloud(unique(word), n, 
                 max.words = 100,
                 random.order = F, colors = brewer.pal(name = "Dark2", n = 8)))

nub2 <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>% 
  count(word, sort = TRUE) 
hacer_nubes(nub2)

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
                                    "años","ai","1", "2018", "cómo", "the", "lunes", "viernes",
                                    "bus003507", "15", "30", "true", TRUE, "_relativeurls",
                                    "	hbspt.cta", "_relativeurls", "hbspt.cta.load", "239039",
                                    "muchas", "veces", "primera", "vez", "quieres", "saber"))




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
  scale_x_reordered() + xlab("bigramas") + labs(title ="Frecuencias de bigramas top 10")+
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip()




################################################################
################## Analisis de correlaciones ###################
###############################################################

load(file = "bases/textominado.RData")


titles <- c("ofertas ciencia de datos", "ofertas estadística", "machine learning",
            "ciencia de datos", "estadística",
            "inteligencia artificial", "big data", "analítica")


limpiartexto <- function(cadena){
  library(tm)
  cadena <- removeNumbers(cadena)
  cadena <- removePunctuation(cadena)
  return(cadena)
}


ofertas_ciencia_de_datos <- limpiartexto (ofertas_ciencia_de_datos)
Ofertas_estadistica <- limpiartexto (Ofertas_estadistica)
machine_learning <- limpiartexto (machine_learning)
ciencia_de_datos <- limpiartexto (ciencia_de_datos)
estadistica <- limpiartexto(estadistica)
inteligencia_artificial <- limpiartexto(inteligencia_artificial)
big_data <- limpiartexto (big_data)
Analitica_de_datos <- limpiartexto (Analitica_de_datos)


books <- list(ofertas_ciencia_de_datos, Ofertas_estadistica, machine_learning,
              ciencia_de_datos, estadistica,
              inteligencia_artificial, big_data, Analitica_de_datos)



## Tokenizar 

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

head(series,18)


#numeros <- as.character(c(1:10))



# calcular el porcentaje de uso de la palabra en todas las novelas
potter_pct <- series %>%  
  anti_join(stop_words_spanish) %>% 
  anti_join(mas_palabras) %>% 
  count(word) %>%
  transmute(word, all_words = n / sum(n))
summary(potter_pct)
colSums(potter_pct[,2])


# calcular el porcentaje de uso de palabras dentro de cada novela
frequency <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>% 
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()


frequency
summary(frequency)
apply(frequency[,3:5], 2, sum)
tail(frequency)

########### compacion para todo el corpus

win.graph()
ggplot(frequency, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "red", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Corpus", x = NULL, title = "Comparación de las frecuencias de palabras")


#### pruebas de correlacion **********

frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)





######### compracion con estadistica

# que tan importante son la palabras para el termino "estadística"

potter_pct2 <- series %>%  
  anti_join(stop_words_spanish) %>% 
  anti_join(mas_palabras) %>% filter(book=="estadística") %>% 
  count(word) %>% 
  transmute(word, all_words = n / 44327 )
dim(potter_pct2)

# calcular el porcentaje de uso de palabras dentro de cada novela
frequency2 <- series %>%
  anti_join(stop_words_spanish) %>%
  anti_join(mas_palabras) %>% 
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct2) %>%
  arrange(desc(book_words)) %>%
  ungroup() %>% filter(!is.na(all_words))# %>% filter(book != "estadística")
apply(frequency2[,3:5], 2, sum)


win.graph()
ggplot(frequency2, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "red", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Estadística", x = NULL, title = "Comparación de las frecuencias de palabras")



frequency2 %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words))
            
 #           ,
#            p_value = cor.test(book_words, all_words)$p.value)





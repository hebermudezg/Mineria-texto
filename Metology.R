

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




# 1. recoleccion  ------------------
texto


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
ggplot(data = tokenizado_ordenado[1:5,], aes(x=word, y=n)) +
  geom_bar(stat = "identity") + coord_flip()






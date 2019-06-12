
################################El empleo########################################

library(rvest)
#library(xml2)

url <- "https://www.elempleo.com/co/ofertas-empleo/trabajo-cientifico-de-datos"
enlaces <- read_html(url) %>%  html_nodes(".result-item a") %>%  xml_attr("href")
enlaces <- paste("https://www.elempleo.com",enlaces, sep = "")

elempleo <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  elempleo[i] <- leer_html %>% html_nodes(".description-block span") %>% html_text()
}


############################## opcionempleo ###########################

url <- "https://www.opcionempleo.com.co/empleo-mineria-de-datos.html"
enlaces <- read_html(url) %>%  html_nodes(".job a") %>%  xml_attr("href")
enlaces <- gsub(pattern = "/job/", "",enlaces)
enlaces <- paste("https://www.opcionempleo.com.co/jobview/", enlaces, sep = "")

opcionempleo <- vector()
for (i in seq(1,length(enlaces),2)){
  leer_html <- read_html(enlaces[i])
  opcionempleo[i] <- leer_html %>% html_nodes(".advertise_compact") %>% html_text()
}



############################# Linkedin ###############################
library(dplyr)

url<-"https://co.linkedin.com/jobs/search?countryRedirected=1&pageNum=0&position=1&keywords=Cient%C3%ADfico%20de%20datos&location=Colombia&currentJobId=1293908240"

enlaces<-read_html(url) %>% html_nodes(".jobs-search-result-item a") %>% html_attr("href")

linkedinES <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  linkedinES[i] <- leer_html %>% html_nodes(".description__text--rich") %>% html_text()
}



# unificando las descripciones de las ofertas de empleo
texto <- c(elempleo, opcionempleo)
#texto2 <- texto  # copia de seguridad
#texto <- texto[1]

save.image(file = "cadenas.RData")
length(texto)




# provemos conbinando los vectores de texto
#texto <-  paste(texto, collapse = ' ')


library(tm)
# limpiando el texto

text_df <- removeWords(texto, words = stopwords("spanish")) # removiendo stop words
text_df <- removePunctuation(text_df) # removiendo puntuaciones
text_df <- removeNumbers(text_df)     # removiendo numeros 
text_df <- stripWhitespace(text_df)   #removiedo espacios en blanco



# creando copus. 
# acervo de documento a analizar.----------------------
nov_corpus <- Corpus(VectorSource(text_df))
nov_corpus[[1]]



# nube de palabras ----------------------------
library(wordcloud)

#nov_corpus %>% unnest_tokens(bigram, nov_corpus, token = "ngrams", n = 2)
wordcloud(nov_corpus, max.words = 200, random.order = F,
          colors = brewer.pal(name = "Dark2", n = 20))




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


########### Agrupamiento jerarquico ## 
nov_new <- removeSparseTerms(nov_tdm, sparse = .95)
nov_new <- nov_new %>% as.matrix()
nov_new <- nov_new[1:50] / rowSums(nov_new)
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")


# coloquemos este texto en un marco de datos (moderno marco de datos)
library(dplyr)
text_df <- tibble(line = 1:length(texto), texto = texto)
text_df


# tokenizar (dividir por n-gramas (ej: palabras))
# nota: elimina puntuacion, convierte en minusculas
library(tidytext)
text_df %>% unnest_tokens(word, texto)








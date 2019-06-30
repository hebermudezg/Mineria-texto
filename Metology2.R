
# librerias
library(tm)
library(wordcloud)


# cargando documentos recuperados mediante web scraping.
# cada termino buscado representa un docuento en nustro contexto
load(file = "textominado.RData")



# funcion para colapsar vecotres

colapsar <- function(cadenas){
  return(paste(cadenas, collapse = " "))
}

Analitica_de_datos2 <- colapsar(Analitica_de_datos)
big_data2 <- colapsar(big_data)
ciencia_de_datos2 <- colapsar(ciencia_de_datos)
estadistica2 <- colapsar(estadistica)
inteligencia_artificial2 <- colapsar(inteligencia_artificial)
machine_learning2 <- colapsar(machine_learning)
ofertas_ciencia_de_datos2 <- colapsar(ofertas_ciencia_de_datos)
Ofertas_estadistica2  <- colapsar(Ofertas_estadistica)



# Corpus (lo priero seria crear un corpus)
Corpus_term <-  Corpus(VectorSource(c(Analitica_de_datos2,
                                 big_data2,
                                 ciencia_de_datos2,
                                 estadistica2,
                                 inteligencia_artificial2,
                                 machine_learning2,
                                 ofertas_ciencia_de_datos2,
                                 Ofertas_estadistica2)))
#inspect(Corpus_term)
content(Corpus_term[[1]])

# Limpiando Corpus

# convirtiendo en minusculas
Corpus_term <- tm_map(Corpus_term, tolower)

# remiviendo puntuaciones 
Corpus_term <- tm_map(Corpus_term, removePunctuation)

# removiendo stopwors
Corpus_term <- tm_map(Corpus_term, removeWords, stopwords("spanish"))


# removiendo otras palabras
otras_palabras <- c("said","from","what")
Corpus_term <- tm_map(Corpus_term, removeWords, otras_palabras)


# removiendo numeros
Corpus_term <- tm_map(Corpus_term, removeNumbers)

# removiendo espacios en blanco 
Corpus_term <- tm_map(Corpus_term, stripWhitespace)



# remover mediante expresiones regulares. 
tm_map(Corpus_term, gsub, patte ="patron", replacement =" ")


# stemming o lematizacion (buscar en español) ********************
#Corpus_term <- tm_map(Corpus_term, stemDocument) 



##   Matriz de terminos y documentos 
Corpus_frecu <- TermDocumentMatrix(Corpus_term)
Corpus_frecu #imprime atributos de frequencies
inspect(Corpus_frecu[1:20, 1:3]) #zoop de la matriz / posiciones [15:20, 5:10]

Corpus_frecu_mat <- as.matrix(Corpus_frecu)

######################

comparar <- as.data.frame(Corpus_frecu_mat_limpio_mat)
names(comparar)
comparar$total <- apply(comparar, 1, sum)





colnames(Corpus_frecu_mat) <- c("Analitica_de_datos",
                                "big_data", "ciencia_de_datos",
                                 "estadistica", "inteligencia_artificial",
                                 "machine_learning", "ofertas_ciencia_de_datos",
                                 "Ofertas_estadistica")
Corpus_frecu_mat_limpio <- removeSparseTerms(Corpus_frecu, 0.8)
Corpus_frecu_mat_limpio_mat <- as.matrix(Corpus_frecu_mat_limpio)
colnames(Corpus_frecu_mat_limpio_mat) <- c("Analitica_de_datos",
                                           "big_data", "ciencia_de_datos",
                                           "estadistica", "inteligencia_artificial",
                                           "machine_learning", "ofertas_ciencia_de_datos",
                                           "Ofertas_estadistica")





aa <- TermDocumentMatrix(Corpus_term, list(weighting=weightTfIdf))
Corpus_frecu_mat_limpio <- removeSparseTerms(aa, 0.8)
Corpus_frecu_mat_limpio_mat <- as.matrix(Corpus_frecu_mat_limpio)
colnames(Corpus_frecu_mat_limpio_mat) <- c("Analitica_de_datos",
                                           "big_data", "ciencia_de_datos",
                                           "estadistica", "inteligencia_artificial",
                                           "machine_learning", "ofertas_ciencia_de_datos",
                                           "Ofertas_estadistica")


# -- STEP 3 : make the graphics !

# Graph 1 : first top 500 discriminant words
#png("#102_1_comparison_cloud_top_500_words.png", width = 480, height = 480)
win.graph()
comparison.cloud(Corpus_frecu_mat_limpio_mat, max.words=500, random.order=FALSE,c(4,0.4), title.size=0.8, rot.per = .3,
                 colors = brewer.pal(10, "Paired"))
#dev.off()

# Graph 2 : first top 2000 discriminant words
#png("#102_1_comparison_cloud_top_2000_words.png", width = 480, height = 480)
win.graph()
comparison.cloud(Corpus_frecu_mat_limpio_mat,max.words=2000,random.order=FALSE,c(4,0.4), title.size=1.4)
#dev.off()

# Graph 3: commonality word cloud : first top 2000 common words across classes
#png("#103_commonality_wordcloud.png", width = 480, height = 480)
win.graph()
commonality.cloud(Corpus_frecu_mat_limpio_mat, max.words=2000, random.order=FALSE)
#dev.off()


#Corpus_frecu_mat_limpio_mat$frecuencias_totales <- apply(Corpus_frecu_mat_limpio_mat, 1, sum)

Corpus_frecu_mat_limpio_mat[1:10, 1:6]


comparison.cloud(Corpus_frecu_mat_limpio_mat, random.order = FALSE, min.freq = 2,
                 layout=TRUE, colors = brewer.pal(10, "Paired"), title.size = 0.8,
                 max.words = 9000, rot.per = .3) #Dark2

hacer_nubes <- function(n){
  library(wordcloud)
  n <- wordcloud(n$palabras, n$frecuencia, min.freq = 2, random.order = FALSE,
                 random.color = FALSE, max.words = 500, colors = brewer.pal(8,"Dark2" ))
}





# removiendo palabras muy grandes y palabras muy pequeñas
# remove words in term matrix with length < 4
index <- as.logical(sapply(rownames(document_tm_clean_mat), function(x) (nchar(x)>3) ))
document_tm_clean_mat_s <- document_tm_clean_mat[index,]



# en cuantos documentos aparecen las palabras. 
findFreqTerms(Corpus_term)
tdm <- TermDocumentMatrix(Corpus_term)
rowSums(as.matrix(tdm)>0)




## Document Clustering with R

library(tm)
library(proxy)


m  <- as.matrix(Corpus_frecu)
colnames(m) <- c("Analitica_de_datos",
                                           "big_data", "ciencia_de_datos",
                                           "estadistica", "inteligencia_artificial",
                                           "machine_learning", "ofertas_ciencia_de_datos",
                                           "Ofertas_estadistica")
distMatrix <- dist(t(m), method="euclidean")

# agrupameitnto jerarquico  CLUSTER 

groups <- hclust(distMatrix,method="ward.D")
win.graph()
plot(groups, cex=0.9, hang=-1)
rect.hclust(groups, k=4)



## ANALISI DE COMPONENTES PRINCIPALES



articleDtm <- DocumentTermMatrix(Corpus_term)
articleDtm2 <- as.matrix(articleDtm)


inspect(articleDtm)
articles_mat <- as.matrix(articleDtm)
rownames(articles_mat) <- c("Analitica_de_datos",
                            "big_data", "ciencia_de_datos",
                            "estadistica", "inteligencia_artificial",
                            "machine_learning", "ofertas_ciencia_de_datos",
                            "Ofertas_estadistica")

distMatrix <- dist(articles_mat, method="euclidean")
groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.9)
rect.hclust(groups, k=3)


library(FactoMineR)
PCA(t(articles_mat))





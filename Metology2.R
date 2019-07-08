
# librerias
library(tm)
library(wordcloud)


# cargando documentos recuperados mediante web scraping.
# cada termino buscado representa un docuento en nustro contexto
load(file = "bases/textominado.RData")



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

## Inspeccionando Corpus
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
otras_palabras <- c("the", "hbsptctarelativeurlstruehbsptctaload", "...",
                    "adelantadorecopilación", "lopez", "for", "lopez")
Corpus_term <- tm_map(Corpus_term, removeWords, otras_palabras)

# removiendo numeros
Corpus_term <- tm_map(Corpus_term, removeNumbers)

# removiendo espacios en blanco 
Corpus_term <- tm_map(Corpus_term, stripWhitespace)


# remover mediante expresiones regulares. 
#tm_map(Corpus_term, gsub, patte ="patron", replacement =" ")


# stemming o lematizacion (buscar en español) ********************
#Corpus_term <- tm_map(Corpus_term, stemDocument) 


##   Matriz de terminos y documentos 
Corpus_frecu <- TermDocumentMatrix(Corpus_term)


## eliminado palabras largas



Corpus_frecu #imprime atributos de frequencies
inspect(Corpus_frecu[1:20, 1:3]) #zoop de la matriz / posiciones [15:20, 5:10]

Corpus_frecu_mat <- as.matrix(Corpus_frecu)
#View(Corpus_frecu_mat)
t(Corpus_frecu_mat[1:10, 1:8])



colnames(Corpus_frecu_mat) <- c("Analitica_de_datos",
                                "big_data", "ciencia_de_datos",
                                 "estadistica", "inteligencia_artificial",
                                 "machine_learning", "ofertas_ciencia_de_datos",
                                 "Ofertas_estadistica")

#Corpus_frecu_mat_limpio <- removeSparseTerms(Corpus_frecu, 0.8)
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
comparison.cloud(Corpus_frecu_mat_limpio_mat, max.words=500, random.order=FALSE, c(4,0.4), title.size=0.8, rot.per = .3,
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

Corpus_frecu_mat_limpio_mat[1:10, 1:8]


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




######### mas clustar 

# vector of colors labelColors = c('red', 'blue', 'darkgreen', 'darkgrey',
# 'purple')
labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951")
# cut dendrogram in 4 clusters
clusMember = cutree(groups, 4)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
hcd = as.dendrogram(groups)
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
plot(clusDendro, main = "Cool Dendrogram", type = "triangle")


library(ape)



# add colors randomly
win.graph()
plot(as.phylo(groups), type = "fan", tip.color = hsv(runif(15, 0.65, 
                                                       0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20, 
                                                                                                                                                 0.5, 3), use.edge.length = TRUE, col = "gray80")


# colored dendrogram

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
win.graph()
A2Rplot(groups, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B", 
                                                                  "#4ECDC4", "#556270"))



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

library(Rgraphviz)

library(tm)
plot.TermDocumentMatrix(articleDtm)
plot(articleDtm )



library("tm")
data("crude")

tdm <- TermDocumentMatrix(Corpus_term , control = list(removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE))
plot(tdm, terms = findFreqTerms(tdm, lowfreq = 50), corThreshold = 0.5)




if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")





############## wordcloud 2




library(wordcloud2)
wordcloud2(data = demoFreq)
wordcloud2(Corpus_frecu_mat_limpio_mat)
letterCloud(Corpus_frecu_mat_limpio_mat, word = "R", size = 2)

letterCloud(demoFreq, word = "R")
win.graph()
letterCloud(demoFreq, word = "WORDCLOUD2", wordSize = 1)
letterCloud(demoFreq, word="USA", size = 0.3, fontFamily="Loma",  backgroundColor = 'black')

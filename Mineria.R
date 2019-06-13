##############################################################################
####------------------- Mineria de texto mediante webscraping---------- ######
##############################################################################

# librerias

# rvest es un nuevo paquete que facilita el raspado
# (o recolecci??n) de datos de p??ginas web html, creado por Hadley Wickham
library(rvest)

# El paquete dplyr proporciona una forma bastante ??gil
# de manejar los ficheros de datos de R
library(dplyr) 



##############################################
#####ofertas de empleo ciencia de datos #####
#############################################

#### El empleo 

url <- "https://www.elempleo.com/co/ofertas-empleo/trabajo-cientifico-de-datos"
enlaces <- read_html(url) %>%  html_nodes(".result-item a") %>%  xml_attr("href")
enlaces <- paste("https://www.elempleo.com",enlaces, sep = "")

elempleo <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  elempleo[i] <- leer_html %>% html_nodes(".description-block span") %>% html_text()
}


#### opcionempleo 

url <- "https://www.opcionempleo.com.co/empleo-mineria-de-datos.html"
enlaces <- read_html(url) %>%  html_nodes(".job a") %>%  xml_attr("href")
enlaces <- gsub(pattern = "/job/", "",enlaces)
enlaces <- paste("https://www.opcionempleo.com.co/jobview/", enlaces, sep = "")

opcionempleo <- vector()
for (i in seq(1,length(enlaces),2)){
  leer_html <- read_html(enlaces[i])
  opcionempleo[i] <- leer_html %>% html_nodes(".advertise_compact") %>% html_text()
}


#### Linkedin

url <- "https://co.linkedin.com/jobs/search?countryRedirected=1&pageNum=0&position=1&keywords=Cient%C3%ADfico%20de%20datos&location=Colombia&currentJobId=1293908240"
enlaces<-read_html(url) %>% html_nodes(".jobs-search-result-item a") %>% html_attr("href")

linkedinES <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  linkedinES[i] <- leer_html %>% html_nodes(".description__text--rich") %>% html_text()
}


# uniedo todas las cadenas de texto recolectadas sobre "ofertas de empleo ciencia de datos"
ofertas_empleo <- c(elempleo, opcionempleo, linkedinES) # 68


##############################################
#####ofertas de empleo en estadistica #####
#############################################

# El empleo EST 

url <- "https://www.elempleo.com/co/ofertas-empleo/trabajo-estadistica/"
enlaces <- read_html(url) %>%  html_nodes(".area-bind") %>%  xml_attr("data-url")
enlaces<- paste("https://www.elempleo.com",enlaces, sep = "")

elempleoEST <- vector()
for (i in seq(1,length(enlaces),2)){
  leer_html <- read_html(enlaces[i])
  elempleoEST[i] <- leer_html %>% html_nodes(".description-block span") %>% html_text()
}
# Opcion empleo EST

url<-"https://www.opcionempleo.com.co/empleo-estadistico.html"
enlaces<-read_html(url) %>% html_nodes(".clickable a") %>% html_attr("href")
enlaces<- paste("https://www.opcionempleo.com.co",enlaces,sep = "")

opcionempleoEST <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  opcionempleoEST[i] <- leer_html %>% html_nodes(".advertise_compact") %>% html_text()
}
#Jobisjob EST

url <- "https://www.jobisjob.com.co/estadistica/trabajos"
enlaces <- read_html(url) %>%  html_nodes(".offer a") %>%  xml_attr("href")
enlaces<- enlaces[-c(2,5,9,11,13,15)]
jobisjobEST <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  jobisjobEST[i] <- leer_html %>% html_nodes(".description") %>% html_text()
}

#Jooble EST
url<- "https://co.jooble.org/trabajo-profesional-estadistica"

enlaces<-read_html(url) %>% html_nodes(".paddings a") %>% html_attr("href")
#con expresiones regulares eliminamos los enlaces que contengan company
patron <- '(company)'
trash<-c(grep(pattern = patron, enlaces) )
enlaces<-enlaces[-(trash)]

joobleEST <- vector()
for (i in 1:length(enlaces)){
  leer_html <- read_html(enlaces[i])
  joobleEST[i] <- leer_html %>% html_nodes(".desc_text_paragraph p")%>% html_text()
}

#Unificando las descripciones de las ofertas de empleo de estadistica
Ofertas_estadistica<- c(elempleoEST,opcionempleoEST,jobisjobEST,joobleEST)



##############################################
##### Terminologias ciencias de datos #####
#############################################

#############################
#####machine learning########
#############################

url <- "https://cleverdata.io/que-es-machine-learning-big-data/"
ML1 <- read_html(url) %>% html_nodes("p") %>% html_text()
url <- "https://www.sas.com/es_co/insights/analytics/machine-learning.html"
ML2 <- read_html(url) %>% html_nodes(".cq-colctrl-lt0 , #machine-learning-usersscroll p , h4 , h4 .txt-white , #machine-learning-importancescroll p , h2+ p , p .txt-large") %>% html_text()
url <- "https://es.wikipedia.org/wiki/Aprendizaje_autom%C3%A1tico"
ML3 <- read_html(url) %>% html_nodes("ul+ p , p:nth-child(7) , h3+ p , p+ p , p:nth-child(63) , li li+ li , .mw-redirect , p:nth-child(71) , dd , p:nth-child(1)") %>% html_text()

# uniendo la infomacion sobre "machine learning"
machine_learning <-c(ML1,ML2,ML3)


#############################
#####ciencia de datos########
#############################

url <- "https://www.xataka.com/otros/de-profesion-cientifico-de-datos"
CD1 <- read_html(url) %>% html_nodes("p:nth-child(52) , p:nth-child(51) , .js-post-images-container li , .article-asset-normal+ p") %>% html_text()
url <- "https://medium.com/datos-y-ciencia/qu%C3%A9-diablos-es-ciencia-de-datos-f1c8c7add107"
CD2 <- read_html(url) %>% html_nodes(".sectionLayout--fullWidth+ .sectionLayout--insetColumn , #58ff") %>% html_text()
url <- "https://es.wikipedia.org/wiki/Ciencia_de_datos"
CD3 <- read_html(url) %>% html_nodes("p+ ol li , p+ ul li , p") %>% html_text()

# uniendo la informacion sobre "ciencia de datos"
ciencia_de_datos <- c(CD1, CD2, CD3)


#############################
##########estad??stica########
#############################

url <- "https://es.wikipedia.org/wiki/Estad%C3%ADstica"
E1 <- read_html(url) %>% html_nodes("td li , h3+ ul li , ul+ ul li , p+ ul li , p") %>% html_text()
url <- "https://economipedia.com/definiciones/estadistica.html"
E2 <- read_html(url) %>% html_nodes(".entry-content li , .entry-content p , #main strong") %>% html_text()
url <- "https://www.significados.com/estadistica/"
E3 <- read_html(url) %>% html_nodes("p:nth-child(4) , h3+ p , h2+ p , .desktop-only+ p") %>% html_text()

#uniendo infomacion sobre "estadistica"
estadistica <- c(E1, E2, E3)


##########################################
##########inteligencia artificial ########
#########################################

url <- "https://es.wikipedia.org/wiki/Inteligencia_artificial"
IA1 <- read_html(url) %>% html_nodes("ul+ ul li , p+ ul li , p") %>% html_text()
url <- "https://searchdatacenter.techtarget.com/es/definicion/Inteligencia-artificial-o-AI"
IA2 <- read_html(url) %>% html_nodes("p") %>% html_text()
url <- "https://www.muyinteresante.es/tecnologia/articulo/ventajas-y-riesgos-de-la-inteligencia-artificial-651449483429"
IA3 <- read_html(url) %>% html_nodes("p") %>% html_text()

# uniendo informacion sobre IA
inteligencia_artificial <- c(IA1, IA2, IA3)


##########################################
##########big data#######################
#########################################

url <- "https://www.sas.com/es_co/insights/big-data/what-is-big-data.html"
BD1 <- read_html(url) %>% html_nodes("#dmusersscroll p , h4 , .list-bullet , .cq-colctrl-lt8-c1 p , #dmhistoryscroll div , p .txt-large") %>% html_text()
url <- "https://www.oracle.com/co/big-data/guide/what-is-big-data.html"
BD2 <- read_html(url) %>% html_nodes(".c89w1 p , #cw31panel-0_0 p , .c81v0 li , .c81w1 p") %>% html_text()
url <- "https://www.bit.es/knowledge-center/que-es-big-data-introduccion-a-big-data/"
BD3 <- read_html(url) %>% html_nodes(".col-md-8 main") %>% html_text()

# uniendo infomacion "big data"
big_data <- c(BD1, BD2, BD3)

##############################################################################
##############################################################################
##########Guardamos todas las bases como un obeto RData#######################
##############################################################################
##############################################################################

save(ofertas_empleo,Ofertas_estadistica, machine_learning,ciencia_de_datos,
     estadistica,inteligencia_artificial, big_data,
     file = "textominado.RData")





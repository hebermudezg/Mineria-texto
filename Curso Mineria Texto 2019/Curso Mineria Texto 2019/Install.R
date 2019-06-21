#paquete para trabajar con big data
install.packages("data.table")
install.packages("tidyverse")

#paquetes para trabajar con web APIs
install.packages("httr")
install.packages("jsonlite")

#paquete para trabajar con datos de texto
install.packages("quanteda")
#si al instalar el paquete salen errores leer la seccion "How to Install" en https://quanteda.io/

#paquetes para visualizar datos
install.packages("GGally")
install.packages("ggplot2")

#paquetes para exportar datos
install.packages("openxlsx")

#paquetes para trabajar con datos especificos
install.packages("bit64")
install.packages("lubridate")

#wrapper para trabajar con Web APIs de noticias
#paquete para trabajar con paquetes de desarolladores
install.packages("devtools")
#si al instalar el paquete salen errores la solucion puede ser
# (WINDOWS) instalar Rtools https://cran.r-project.org/bin/windows/Rtools/
# (MAC) instalar Xcode desde App Store 
# (UBUNTU) sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
library(devtools)
install_github("mkearney/newsAPI")
install_github("hrbrmstr/webhose")
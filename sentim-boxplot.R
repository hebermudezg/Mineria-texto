library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

tuits <- read.csv("bases/tweets.csv", stringsAsFactors = F,
                  encoding = "UTF-8") %>%  tbl_df()

afinn <- read.csv("bases/lexico_afinn.en.es.csv", stringsAsFactors = F,
                  fileEncoding = "latin1") %>% 
  tbl_df()

#filtrar por anio
tuits <- tuits %>%
  separate(created, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo","Mes","Dia" ), sep = "-") %>%
  filter(Periodo == 2019)

#tokenizar
tuits_afinn <- tuits %>% 
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 
  #rename("Candidato" = screen_name)

tuits <-
  tuits_afinn %>%
  group_by(id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(tuits, ., by = "status_id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit))  
  #rename("Candidato" = screen_name)

# Total
tuits_afinn %>%
  count(Candidato)

# ??nicas
tuits_afinn %>% 
  group_by(Candidato) %>% 
  distinct(Palabra) %>% 
  count()
#===============================================================================
#TEMA PARA PODER VISUALIZAR
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

#===============================================================================
#veamos asi las palabras positivas y negativas m??s usadas por cada uno de ellos, usando map()

#map(c("Positiva", "Negativa"), 
    
sent <- function(sentimiento) {
  tuits_afinn %>%
  filter(Tipo ==  sentimiento) %>%
  group_by(Candidato) %>%
  count(Palabra, sort = T) %>%
  top_n(n = 10, wt = n) %>%
  ggplot() +
  aes(Palabra, n, fill = Candidato) +
  geom_col() +
  facet_wrap("Candidato", scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = sentimiento) +
  tema_graf
}
sent("Positiva")

#===============================================================================
#Como deseamos observar tendencias, vamos a obtener la media de sentimientos 
#por d??a, usando group_by() y summarise()

tuits_afinn_fecha <-
  tuits_afinn %>%
  #unite(Fecha, Dia, Mes, Periodo,sep = "/")%>% 
  group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Candidato, Dia) %>%
  summarise(Media = mean(Puntuacion))

#Veamos nuestros resultados con ggplot()
#separamos las l??neas por candidato

tuits_afinn_fecha %>%
  ggplot() +
  aes(Dia, Media, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf +
  theme(legend.position = "none")
# lineas por candidato 
tuits_afinn %>%
  ggplot() +
  aes(Dia, Puntuacion, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf
# Con regresion
tuits_afinn_fecha %>%
  ggplot() +
  aes(Dia, Media, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf
#boxplot
tuits %>%
  ggplot() +
  aes(Candidato, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot() +
  tema_graf

#Tambi??n podemos crear boxplots para ver cambios a trav??s del tiempo

tuits %>%
  mutate(Mes = factor(Mes)) %>% 
  ggplot() +
  aes(Mes, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot(width = 1) +
  facet_wrap(~Candidato) +
  tema_graf +
  theme(legend.position = "none")

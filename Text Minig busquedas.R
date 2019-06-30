library(readr)
library(stringr)
library(tm)
library(igraph)

#=============================================
#-------------Directorio de trabajo ----------
#============================================

setwd('../Documents/Simposio/Mineria-texto/Palabras/')

#=============================================
#----------- lectura de variables -----------
#============================================


ciencia_de_datos <- read_csv('ciencia_de_datos_text.txt', col_names = F)
machine_learning <- read_csv('machine_learning_text.txt', col_names = F)
aprendizaje_automatico <- read_csv('aprendizaje_automatico_text.txt', col_names = F)
big_data <- read_csv('big_data_text.txt', col_names = F)
inteligencia_artificial <- read_csv('inteligencia_artificial_text.csv', col_names = F)
analitica_de_datos <- read_csv('analitica_de_datos_text.csv', col_names = F)
mineria_de_datos <- read_csv('mineria_de_datos_text.txt', col_names = F)
inteligencia_de_negocios <- read_csv('inteligencia_de_negocios_text.txt', col_names = F)
estadistica <- read_csv('estadistica_text.txt', col_names = F)


#==========================================
# Eliminar caracteres no informativos
# ===============================

delete_numbers <- function(x){
  y <- str_replace(x , '.*([:digit:]).*', ' ')
  
}





ciencia_de_datos$nuevo <- sapply(ciencia_de_datos$X1, delete_numbers) 
machine_learning$nuevo <- sapply(machine_learning$X1, delete_numbers) 
aprendizaje_automatico$nuevo <- sapply(aprendizaje_automatico$X1, delete_numbers) 
big_data$nuevo <- sapply(big_data$X1, delete_numbers) 
inteligencia_artificial$nuevo <- sapply(inteligencia_artificial$X1, delete_numbers) 
analitica_de_datos$nuevo <- sapply(analitica_de_datos$X1, delete_numbers) 
mineria_de_datos$nuevo <- sapply(mineria_de_datos$X1, delete_numbers) 
inteligencia_de_negocios$nuevo <- sapply(inteligencia_de_negocios$X1, delete_numbers) 
estadistica$nuevo <- sapply(estadistica$X1, delete_numbers) 


# ==============================================
# ------------- Frecuencias -------------------
# =============================================

ciencia_de_datos_freq <- data.frame(table(ciencia_de_datos$nuevo))
quantile(ciencia_de_datos_freq$Freq, 0.95)
ciencia_de_datos_freq <- ciencia_de_datos_freq[ciencia_de_datos_freq$Freq > 5,]
ciencia_de_datos_freq <- ciencia_de_datos_freq[-c(1:6),]

machine_learning_freq <- data.frame(table(machine_learning$nuevo))
quantile(machine_learning_freq$Freq, 0.95)
machine_learning_freq <- machine_learning_freq[machine_learning_freq$Freq>7,]
machine_learning_freq <- machine_learning_freq[-c(1:8),]


aprendizaje_automatico_freq <- data.frame(table(aprendizaje_automatico$nuevo))
quantile(aprendizaje_automatico_freq$Freq, 0.95)
aprendizaje_automatico_freq <- aprendizaje_automatico_freq[aprendizaje_automatico_freq$Freq>5,]
aprendizaje_automatico_freq <- aprendizaje_automatico_freq[-c(1:4),]



big_data_freq <- data.frame(table(big_data$nuevo))
quantile(big_data_freq$Freq, 0.95)
big_data_freq <- big_data_freq[big_data_freq$Freq>8,]
big_data_freq <- big_data_freq[-c(1:6),]


inteligencia_artificial_freq <- data.frame(table(inteligencia_artificial$nuevo))
quantile(inteligencia_artificial_freq$Freq, 0.95)
inteligencia_artificial_freq <- inteligencia_artificial_freq[inteligencia_artificial_freq$Freq > 7,]
inteligencia_artificial_freq <- inteligencia_artificial_freq[-c(1:20),]


analitica_de_datos_freq <- data.frame(table(analitica_de_datos$nuevo))
quantile(analitica_de_datos_freq$Freq, 0.95)
analitica_de_datos_freq <- analitica_de_datos_freq[analitica_de_datos_freq$Freq>9,]
analitica_de_datos_freq <- analitica_de_datos_freq[-c(1:3),]


mineria_de_datos_freq <- data.frame(table(mineria_de_datos$nuevo))
quantile(mineria_de_datos_freq$Freq, 0.95)
mineria_de_datos_freq <- mineria_de_datos_freq[mineria_de_datos_freq$Freq>8,]
mineria_de_datos_freq <- mineria_de_datos_freq[-c(1:5),]



estadistica_freq <- data.frame(table(estadistica$nuevo))
quantile(estadistica_freq$Freq, 0.95)
estadistica_freq <- estadistica_freq[estadistica_freq$Freq>8,]
estadistica_freq <- estadistica_freq[-c(1:6),]

# ==========================================
# ------------------ graficos -----------
# =====================================


#==================ciencia de datos ==============

terminos <- c('datos', 'ciencia', 'data', 'decisiones', 'información'
, 'análisis', 'science', 'científicos', 'estadística',  'científico', 'aprendizaje'
, 'automático', 'empresa', 'trabajo', 'analizar', 'big',  'encontrar',  'algoritmos'
, 'definición'
,     'empresas'
, 'estadísticas'
,  'estadístico'
,     'software'
, 'conocimiento'
,       'equipo'
,'estructurados'
, 'herramientas'
,   'ingeniería'
, 'inteligencia'
,    'analítica'
,   'disciplina'
,     'learning'
,      'machine'
,    'marketing'
,       'python'
,     'técnicas'
, 'aplicaciones'
,     'artículo'
,        'bases'
,     'ciencias'
,  'habilidades'
,    'lenguajes'
,  'matemáticas'
,      'métodos'
,      'minería'
,      'modelos'
, 'organización'
,     'problema'
,      'proceso'
,        'temas'
,        'valor')

ciencia_de_datos_freq  %>% filter(Var1 %in% terminos) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>% arrange(desc(proporcion))  -> data_cd

ggplot(data_cd, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Ciencia de datos') +
  labs(x = 'Palabras', y = 'Proporción')


#==================machine_learning  ==============

terminos_ml <- c('aprendizaje'
,       'datos'
,    'learning'
,     'machine'
,  'automático'
,     'modelos'
,  'algoritmos'
, 'información'
,'inteligencia'
,    'análisis'
,     'métodos'
,'aplicaciones'
,  'decisiones'
,     'proceso'
,    'artículo'
,    'analizar')

machine_learning_freq  %>% filter(Var1 %in% terminos_ml) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))  -> data_ml

ggplot(data_ml, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Machine Learning') +
  labs(x = 'Palabras', y = 'Proporción')

#==================aprendizaje_automatico  ==============

terminos_aa <- c(
    'aprendizaje'
    ,      'datos'
    , 'algoritmos'
    , 'automático'
    ,   'learning'
    ,'supervisado'
    ,    'machine'
    ,    'métodos'
    ,     'modelo'
    ,    'modelos'
    ,  'algoritmo'
    ,   'análisis'
    ,        'sas'
    ,   'aprender'
    ,    'máquina'
    , 'decisiones' 
    ,'etiquetados'
    ,   'patrones'
    ,    'proceso'
    , 'artificial'
    ,'conocimiento'
    ,  'encontrar'
    ,'inteligencia'
    ,   'objetivo'
    ,  'resultado'
   ,'casificación'
    ,'computadoras'
    ,   'conjunto'
   ,'entrenamiento')

aprendizaje_automatico_freq   %>% filter(Var1 %in% terminos_aa) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))  -> data_aa

ggplot(data_aa, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Aprendizaje Automatico') +
  labs(x = 'Palabras', y = 'Proporción')


#==================big_data  ==============

terminos_bd <- c('data'
  ,           'datos'
  ,             'big'
  ,     'información'
  ,        'análisis'
  ,        'sistemas'
  ,     'information'
  ,       'seguridad'
  ,       'conjuntos'
  ,  'almacenamiento'
  ,        'empresas'
  ,   'estructurados'
  ,          'nuevas'
  ,         'volumen'
  ,       'analytics'
  ,     'tecnologías'
  ,          'hadoop'
  ,      'tecnología'
  ,      'decisiones'
  ,   'procesamiento'
  ,       'productos'
  ,            'sets'
  ,        'software'
  ,           'bases'
  ,        'internet'
  ,       'problemas'
  ,        'analysis'
  ,    'herramientas'
  ,      'resultados'
  ,   'investigación'
  ,        'sociales'
  ,  'organizaciones'
  ,        'research'
  ,         'science'
  ,         'sistema'
  ,        'analizar'
  ,     'científicos'
  ,      'desarrollo'
  ,             'sas'
  , 'características'
  ,       'almacenar'
  ,       'analítica'
  ,          'apache'
  ,        'business'
  ,       'computing'
  ,          'future'
  ,       'industria'
  ,         'modelos'
  ,        'negocios'
  ,         'proceso'
  ,        'procesos'
  ,      'soluciones'
  ,    'arquitectura'
  ,       'framework'
  ,    'inteligencia'
  ,      'macrodatos'
  ,           'media'
  ,        'proyecto'
  ,      'technology'
  
  
)

big_data_freq  %>% filter(Var1 %in% terminos_bd) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))   -> data_bd

ggplot(data_bd, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Big Data') +
  labs(x = 'Palabras', y = 'Proporción')

#==================inteligencia_artificial  ==============

terminos_ia <- c('inteligencia'
  ,             'ia'
  ,          'datos'
  ,       'sistemas'
  ,       'programa'
  ,        'sistema'
  ,       'clientes'
  ,   'inteligentes'
  ,        'cliente'
  ,   'conocimiento'
  ,    'información'
  ,       'máquinas'
  ,      'problemas'
  ,        'máquina'
  ,         'robots'
  ,       'problema'
  ,     'decisiones'
  ,  'internacional'
  , 'reconocimiento'
  ,          'temas'
  ,      'capacidad'
  ,        'ciencia'
  ,       'lenguaje'
  ,       'noticias'
  ,       'patrones'
  ,          'robot'
  ,         'tareas'
  ,       'concepto'
  ,    'inteligente'
  ,        'modelos'
  ,       'procesos'
  ,      'programas'
  ,     'tecnología')

inteligencia_artificial_freq %>% filter(Var1 %in% terminos_ia) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))  -> data_ia

ggplot(data_ia, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Inteligencia Artificial') +
  labs(x = 'Palabras', y = 'Proporción')

#==================analitica_de_datos  ==============

terminos_ad <- c( 'datos'
  ,      'analítica'
  ,    'información'
  ,       'análisis'
  ,       'clientes'
  ,          'valor'
  ,        'empresa'
  ,       'empresas'
  ,           'data'
  ,      'productos'
  ,            'big'
  ,        'consumo'
  ,      'servicios'
  ,     'producción'
  ,        'grandes'
  ,       'producto'
  ,     'generación'
  ,  'investigación'
  ,     'estrategia'
  ,    'necesidades'
  ,  'profesionales'
  ,       'maestría'
  ,   'conocimiento'
  ,   'inteligencia'
  ,       'procesos'
  ,       'servicio'
  ,        'cliente'
  ,    'empresarial'
  ,       'ingresos'
  ,        'negocio'
  ,        'proceso'
  ,     'beneficios'
  ,       'compañía'
  ,        'conocer'
  , 'organizaciones'
  ,        'digital'
  ,       'negocios'
  ,    'principales'
  ,     'decisiones'
  ,   'herramientas'
  ,            'ibm'
  ,      'marketing'
  ,        'métodos'
  ,         'modelo'
  ,        'modelos'
  ,   'optimización'
  ,       'patrones'
  ,          'pymes')

analitica_de_datos_freq %>% filter(Var1 %in% terminos_ad) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion)) -> data_ad

ggplot(data_ad, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Analitica de Datos') +
  labs(x = 'Palabras', y = 'Proporción')

#==================mineria_de_datos  ==============

terminos_md <- c( 'datos'
  ,        'minería'
  ,    'información'
  ,       'análisis'
  ,       'clientes'
  ,       'técnicas'
  ,       'patrones'
  ,       'sistemas'
  ,        'cliente'
  ,           'data'
  ,        'modelos'
  ,   'conocimiento'
  ,     'datamining'
  ,        'grandes'
  ,         'mining'
  ,     'resultados'
  ,          'bases'
  ,         'modelo'
  ,        'proceso'
  ,     'decisiones'
  ,       'negocios'
  ,     'relaciones'
  ,     'tecnología'
  ,   'inteligencia'
  ,   'aplicaciones'
  ,      'productos'
  ,       'software'
  ,            'sas'
  ,        'empresa'
  ,  'visualización'
  ,     'artificial'
  ,       'conjunto'
  ,      'objetivos'
  ,        'soporte'
  ,     'algoritmos'
  ,           'base'
  ,         'compra'
  ,   'estadísticos'
  ,      'marketing'
  ,       'personas'
  ,       'predecir'
  ,       'producto'
  ,       'proyecto'
  , 'comportamiento'
  ,       'decisión'
  ,       'detectar'
  ,       'empresas'
  ,    'estadística'
  ,        'gráfica'
  ,        'negocio'
  ,        'resumen'
  ,     'tendencias'
  ,         'visión')

mineria_de_datos_freq  %>% filter(Var1 %in% terminos_md) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))  -> data_md

ggplot(data_md, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Mineria de Datos') +
  labs(x = 'Palabras', y = 'Proporción')


#==================estadistica  ==============

terminos_e <- c('estadística'
  ,          'datos'
  ,      'población'
  ,        'muestra'
  ,        'estudio'
  ,    'información'
  ,         'número'
  ,   'estadísticas'
  ,         'escala'
  ,      'elementos'
  ,        'valores'
  ,   'estadísticos'
  ,        'métodos'
  ,       'conjunto'
  ,       'variable'
  ,       'análisis'
  ,        'números'
  ,    'estudiantes'
  ,     'resultados'
  ,        'ciencia'
  ,      'variables'
  ,       'medición'
  ,         'medida'
  ,        'medidas'
  ,         'método'
  ,       'personas'
  ,'características'
  ,       'muestras'
  ,  'observaciones'
  ,       'ciencias'
  ,          'tipos'
  ,          'valor'
  ,   'conclusiones'
  ,     'diferentes'
  ,    'estadístico'
  ,  'investigación'
  ,        'errores'
  ,       'estudios'
  ,     'frecuencia'
  ,        'iguales'
  ,     'intervalos'
  ,   'probabilidad'
  ,         'series'
  ,  'cuantitativos'
  ,       'objetivo'
  ,    'observación'
  ,        'proceso'
  ,     'decisiones'
  ,    'descriptiva'
  ,       'magnitud'
  ,       'muestreo'
  ,       'sociales'
  ,       'técnicas'
  ,      'atributos'
  ,       'concepto'
  ,         'efecto'
  ,        'fuentes'
  ,      'hipótesis'
  ,        'ordinal'
  ,         'teoría')

estadistica_freq  %>% filter(Var1 %in% terminos_e) %>% 
  mutate(proporcion = Freq/sum(Freq)) %>%
  arrange(desc(proporcion))  -> data_e

ggplot(data_e, aes(x = reorder(Var1, -proporcion), y = proporcion)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Proporcion palabras en busqueda Estadística') +
  labs(x = 'Palabras', y = 'Proporción')

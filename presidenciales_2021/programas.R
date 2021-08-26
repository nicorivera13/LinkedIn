###########################################################
# Análisis de los programas candidatos presidenciales 2021
# por Nicolas Rivera
###########################################################

# Paquetes
library(tidyverse)
library(ggplot2)
library(pdftools) 
library(tidytext)
library(stringr)
library(readr)
library(ggpubr)

# Opciones
options(scipen=999)

# Directorio de trabajo
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/programas_candidatos_presidenciales')

# Se cargan los discursos

boric <- pdf_text('programa_boric.pdf')
boric <- paste(boric, collapse = " ")
boric <- str_remove_all(boric, "\n[:space:]+[:digit:]+\n")

sichel <- pdf_text('programa_sichel_v2.pdf')
sichel <- paste(sichel, collapse = " ")
sichel <- str_remove_all(sichel, "\n[:space:]+[:digit:]+\n")

provoste <- pdf_text('programa_provoste.pdf')
provoste <- paste(provoste, collapse = " ")
provoste <- str_remove_all(provoste, "\n[:space:]+[:digit:]+\n")

JAK <- pdf_text('programa_jak.pdf')
JAK <- paste(JAK, collapse = " ")
JAK <- str_remove_all(JAK, "\n[:space:]+[:digit:]+\n")

# Convertir texto en data frame

freq_boric <- tibble(programa = boric) %>% 
  unnest_tokens(output = palabra, 
                input = programa, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) 

freq_sichel <- tibble(programa = sichel) %>% 
  unnest_tokens(output = palabra, 
                input = programa, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) 

freq_provoste <- tibble(programa = provoste) %>% 
  unnest_tokens(output = palabra, 
                input = programa, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) 

freq_JAK <- tibble(programa = JAK) %>% 
  unnest_tokens(output = palabra, 
                input = programa, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) 

# Stopwords

stopwords_es <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

mis_stopwords <- tibble(palabra = c('gobierno','programa','ser','debe','años','mayor','iv','vi','vii','viii','ix','delas','delos','yasna','provoste','estimamos','quese','tengamos'))

# Frecuencias -------------------------------------------------

grafico_boric <- freq_boric %>% 
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) + 
  geom_col(fill = '#C1403D') +
  theme_minimal() +
  labs (title = 'Gabriel Boric',
        y = NULL,
        x = "Frecuencia") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

grafico_provoste <- freq_provoste %>% 
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) + 
  geom_col(fill = '#A79C93') +
  theme_minimal() +
  labs (title = 'Yasna Provoste',
        y = NULL,
        x = "Frecuencia") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

grafico_sichel <- freq_sichel %>% 
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) + 
  geom_col(fill = '#0294A5') +
  theme_minimal() +
  labs (title = 'Sebastián Sichel',
        y = NULL,
        x = "Frecuencia") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

grafico_JAK <- freq_JAK %>% 
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) + 
  geom_col(fill = '#03353E') +
  theme_minimal() +
  labs (title = 'José Antonio Kast',
        y = NULL,
        x = "Frecuencia") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

final <- ggarrange(grafico_boric, 
                   grafico_provoste,
                   grafico_sichel,
                   grafico_JAK,
                   ncol = 2, 
                   nrow = 2, 
                   common.legend = TRUE, 
                   legend = 'bottom')

final <- annotate_figure(final, 
                         top = text_grob('Figura 1. Palabras más frecuentes en los programas de gobierno de los candidatos presidenciales 2021', 
                                         color = 'Black',
                                         face = 'bold', 
                                         size = 16), 
                         bottom = text_grob('Fuente: Elaboración propia utilizando los programas de gobierno de los candidatos presidenciales, disponibles en www.boricpresidente.cl, www.yasnapresidenta.cl, www.sumamosxsichel.cl, y www.atrevetechile.cl.',
                                            color = 'Black',
                                            face = 'italic', 
                                            size = 10))

ggsave('frecuencia.png')

# TF-IDF -------------------------------------------------

freq_boric <- freq_boric %>% 
  mutate(candidato = 'Boric', .before = palabra) 

freq_provoste <- freq_provoste %>% 
  mutate(candidato = 'Provoste', .before = palabra) 

freq_sichel <- freq_sichel %>% 
  mutate(candidato = 'Sichel', .before = palabra) 

freq_JAK <- freq_JAK %>% 
  mutate(candidato = 'JAK', .before = palabra) 

freq_all <- bind_rows(freq_boric, freq_provoste,
                      freq_sichel, freq_JAK) %>% 
            anti_join(stopwords_es) %>% 
            anti_join(mis_stopwords) 

tfidf <- bind_tf_idf(freq_all,
                     term = palabra, 
                     document = candidato,
                     n = n)

tfidf_boric <- tfidf %>% 
  filter(candidato == 'Boric') %>% 
  arrange(desc(tf_idf)) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), tf_idf)) + 
  geom_col(fill = '#C1403D') +
  theme_minimal() +
  labs (title = 'Gabriel Boric',
        y = NULL,
        x = "tf_idf") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

tfidf_provoste <- tfidf %>% 
  filter(candidato == 'Provoste') %>% 
  arrange(desc(tf_idf)) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), tf_idf)) + 
  geom_col(fill = '#A79C93') +
  theme_minimal() +
  labs (title = 'Yasna Provoste',
        y = NULL,
        x = "tf_idf") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))
  
tfidf_sichel <- tfidf %>% 
  filter(candidato == 'Sichel') %>% 
  arrange(desc(tf_idf)) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), tf_idf)) + 
  geom_col(fill = '#0294A5') +
  theme_minimal() +
  labs (title = 'Sebastián Sichel',
        y = NULL,
        x = "tf_idf") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

tfidf_JAK <- tfidf %>% 
  filter(candidato == 'JAK') %>% 
  arrange(desc(tf_idf)) %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), tf_idf)) + 
  geom_col(fill = '#03353E') +
  theme_minimal() +
  labs (title = 'José Antonio Kast',
        y = NULL,
        x = "tf_idf") +
  theme(plot.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

final2 <- ggarrange(tfidf_boric, 
                    tfidf_provoste,
                    tfidf_sichel,
                    tfidf_JAK,
                    ncol = 2, 
                    nrow = 2, 
                    common.legend = TRUE, 
                    legend = 'bottom')

final2 <- annotate_figure(final2, 
                         top = text_grob('Figura 2. Palabras más relevantes de cada programa presidencial respecto al de los otros candidatos', 
                                         color = 'Black',
                                         face = 'bold', 
                                         size = 16), 
                         bottom = text_grob('Fuente: Elaboración propia utilizando los programas de gobierno de los candidatos presidenciales, disponibles en www.boricpresidente.cl, www.yasnapresidenta.cl, www.sumamosxsichel.cl, y www.atrevetechile.cl.',
                                            color = 'Black',
                                            face = 'italic', 
                                            size = 10))

ggsave('tf_idf.png')

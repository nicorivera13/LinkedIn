##################################
# Top 5 causes of death in Chile 
# by Nicolas Rivera
##################################

# Packages
library('tidyverse')
library('ggplot2')
library('readxl')
library('ggsci')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/top5_causes_death')

# Importing data
defunciones <- read_csv2('DEFUNCIONES_2016_2021.csv', col_names = FALSE)

colnames(defunciones) <- c('ANO_DEF',"FECHA_DEF",
                           "GLOSA_SEXO", "EDAD_TIPO", "EDAD_CANT", "CODIGO_COMUNA_RESIDENCIA", "GLOSA_COMUNA_RESIDENCIA", "GLOSA_REG_RES", "DIAG1", "CAPITULO_DIAG1", "GLOSA_CAPITULO_DIAG1", "CODIGO_GRUPO_DIAG1", "GLOSA_GRUPO_DIAG1", "CODIGO_CATEGORIA_DIAG1", "GLOSA_CATEGORIA_DIAG1", "CODIGO_SUBCATEGORIA_DIAG1", "GLOSA_SUBCATEGORIA_DIAG1", "DIAG2", "CAPITULO_DIAG2", "GLOSA_CAPITULO_DIAG2", "CODIGO_GRUPO_DIAG2", "GLOSA_GRUPO_DIAG2", "CODIGO_CATEGORIA_DIAG2", "GLOSA_CATEGORIA_DIAG2", "CODIGO_SUBCATEGORIA_DIAG2", "GLOSA_SUBCATEGORIA_DIAG2", "LUGAR_DEFUNCION")

# Deaths until july 2021
defunciones <- defunciones %>% 
  filter(FECHA_DEF <= '2021-07-31')

summary(defunciones$FECHA_DEF)

# Deaths per year
total <- defunciones %>% 
  group_by(ANO_DEF) %>% 
  summarise(total = n())

top5 <- defunciones %>% 
  group_by(ANO_DEF, GLOSA_CATEGORIA_DIAG1) %>% 
  summarise(DEF = n()) %>% 
  slice_max(DEF, n = 5) %>% 
  left_join(total, by = 'ANO_DEF') %>% 
  mutate(prop = DEF/total*100)

# Renaming categories
sort(unique(top5$GLOSA_CATEGORIA_DIAG1))

top5$GLOSA_CATEGORIA_DIAG1[grepl('Hipertensi', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Hipertensión esencial'

top5$GLOSA_CATEGORIA_DIAG1[grepl('Neumon', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Neumonía, organismo no especificado'

top5$GLOSA_CATEGORIA_DIAG1[grepl('Otras enfermedades pulmonares obstructivas', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Otras enfermedades pulmonares obstructivas crónicas'

top5$GLOSA_CATEGORIA_DIAG1[grepl('Tumor maligno de los bronquios', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Tumor maligno de los bronquios y del pulmón'

top5$GLOSA_CATEGORIA_DIAG1[grepl('Tumor maligno del est', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Tumor maligno del estómago'

top5$GLOSA_CATEGORIA_DIAG1[grepl('Uso emergente de U07', top5$GLOSA_CATEGORIA_DIAG1)] <- 'Covid-19'

table(top5$GLOSA_CATEGORIA_DIAG1)

# Building figure
ggplot(top5, aes(x = as.factor(ANO_DEF), 
                 y = DEF, 
                 fill = GLOSA_CATEGORIA_DIAG1,DEF)) + 
  geom_bar(stat='identity', position='dodge', colour='black', size=.3) +
  scale_y_continuous(labels = function(x) format(x, big.mark = '.')) +
  xlab('Año') + ylab('Número de fallecidos') + 
  labs(title = 'Principales 5 causas de muerte en Chile según año',
       subtitle = '(2016-2021)',
       caption = 'Notas: (1) Se consideran registros hasta el 31 de julio de 2021. (2) Causas de muerte agrupadas según categorías CIE-10. (3) Covid-19 incluye virus identificado y no identificado. Fuente: DEIS, Minsal.') +
  guides(fill=guide_legend(title='')) +
  theme_bw() +
  theme(legend.position= 'bottom') +
  scale_fill_npg() +
  theme(plot.caption = element_text(face = 'italic',
                                    size = 8),
        plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank())

# Exporting figure
ggsave('top5_causes_death.png')
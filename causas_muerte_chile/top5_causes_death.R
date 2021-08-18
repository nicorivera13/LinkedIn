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
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/Twitter/top5_causes_death/new')

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

# Top 5 causes of death per year
defunciones$GLOSA_SUBCATEGORIA_DIAG1[defunciones$GLOSA_SUBCATEGORIA_DIAG1=='COVID-19 virus identificado'] <- 'Covid-19'
defunciones$GLOSA_SUBCATEGORIA_DIAG1[defunciones$GLOSA_SUBCATEGORIA_DIAG1=='COVID-19 virus no identificado'] <- 'Covid-19'

top5 <- defunciones %>% 
  group_by(ANO_DEF, GLOSA_SUBCATEGORIA_DIAG1) %>% 
  summarise(DEF = n()) %>% 
  slice_max(DEF, n = 5) %>% 
  left_join(total, by = 'ANO_DEF') %>% 
  mutate(prop = DEF/total*100)

sort(unique(top5$GLOSA_SUBCATEGORIA_DIAG1))

top5$GLOSA_SUBCATEGORIA_DIAG1[top5$GLOSA_SUBCATEGORIA_DIAG1=='HIPERTENSION ESENCIAL (PRIMARIA)'] <- 'Hipertensión esencial'
top5$GLOSA_SUBCATEGORIA_DIAG1[top5$GLOSA_SUBCATEGORIA_DIAG1=='INFARTO AGUDO DEL MIOCARDIO SIN OTRA ESPECIFICACION'] <- 'Infarto al corazón'
top5$GLOSA_SUBCATEGORIA_DIAG1[top5$GLOSA_SUBCATEGORIA_DIAG1=='NEUMONIA NO ESPECIFICADA'] <- 'Neumonía'
top5$GLOSA_SUBCATEGORIA_DIAG1[top5$GLOSA_SUBCATEGORIA_DIAG1=='TUMOR MALIGNO DEL ESTOMAGO PARTE NO ESPECIFICADA'] <- 'Cáncer de estómago'

top5$GLOSA_SUBCATEGORIA_DIAG1[grepl('TUMOR MALIGNO DE LOS BRONQUIOS', top5$GLOSA_SUBCATEGORIA_DIAG1)] <- 'Cáncer de bronquios o pulmón'

table(top5$GLOSA_SUBCATEGORIA_DIAG1)

# Building figure
ggplot(top5, aes(x = as.factor(ANO_DEF), 
                 y = DEF, 
                 fill = GLOSA_SUBCATEGORIA_DIAG1,DEF)) + 
  geom_bar(stat='identity', position='dodge', colour='black', size=.3) +
  scale_y_continuous(labels = function(x) format(x, big.mark = '.')) +
  xlab('Año') + ylab('Número de fallecidos') + 
  labs(title = 'Principales 5 causas de muerte en Chile',
       subtitle = '(2016-2021)',
       caption = 'Notas: (1) Se consideran registros hasta el 31 de julio de 2021. (2) La categoría "Covid-19" incluye virus identificado y no identificado. Fuente: DEIS, Minsal.') +
  guides(fill=guide_legend(title='')) +
  theme_bw() +
  theme(legend.position= 'bottom') +
  scale_fill_npg() +
  theme(plot.caption = element_text(face = 'italic',
                                    size = 6.5),
        plot.title = element_text(face = 'bold'),
        panel.grid.minor = element_blank())

# Exporting figure
ggsave('top5_causes_death.png')

#################################
# Nombres Axé 
# por Nicolas Rivera
#################################

# Paquetes
library('tidyverse')
library('ggplot2')

#install.packages('guaguas')
library('guaguas')

library('ggsci')

#install.packages('gganimate')
library('gganimate')

# Directorio de trabajo
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/nombres_axe')

# Cargar datos
nombres <- guaguas

# Unificar nombres
nombres$nombre[nombres$nombre == 'Francinie'] <- 'Francini'
nombres$nombre[nombres$nombre == 'Fransini'] <- 'Francini'
nombres$nombre[nombres$nombre == 'Francinni'] <- 'Francini'
nombres$nombre[nombres$nombre == 'Francinnie'] <- 'Francini'
nombres$nombre[nombres$nombre == 'Franchini'] <- 'Francini'

nombres$nombre[nombres$nombre == 'Yeferson'] <- 'Jefferson'
nombres$nombre[nombres$nombre == 'Yefersson'] <- 'Jefferson'
nombres$nombre[nombres$nombre == 'Jeferson'] <- 'Jefferson'
nombres$nombre[nombres$nombre == 'Jefersonn'] <- 'Jefferson'

nombres$nombre[nombres$nombre == 'Flabiana'] <- 'Flaviana'
nombres$nombre[nombres$nombre == 'Flavianna'] <- 'Flaviana'

nombres$nombre[nombres$nombre == 'Fabriccio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabrisio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabrissio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabritzio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabritzzio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabritcio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabrizio'] <- 'Fabricio'
nombres$nombre[nombres$nombre == 'Fabrizzio'] <- 'Fabricio'

# Agregar años faltantes
nombres <- nombres %>%
  add_row(anio = 1999, nombre = 'Francini', n = 0) %>%
  add_row(anio = 2000, nombre = 'Francini', n = 0) %>%
  add_row(anio = 2001, nombre = 'Francini', n = 0)

# Sumar nombres x año
nombres <- nombres %>% 
  group_by(anio, nombre) %>% 
  summarise(N = sum(n))

# Nombres de integrantes de grupos Axé
axe_hombres <- c('Jefferson', 'Fabricio')
axe_mujeres <- c('Flaviana', 'Francini')

nombres_axe <- nombres %>% 
  filter((nombre %in% axe_hombres) | (nombre %in% axe_mujeres)) %>% 
  filter(anio>=1990 & anio<=2010)

# Figura

nombres_axe  %>%
  ggplot(aes(x=anio, y=N, group=nombre, color=nombre)) +
  geom_line() +
  geom_point() +
  labs(x='Año',
       y = 'Número de bebés',
       color = '',
       title = 'Bebés inscritos con nombres de integrantes de grupos Axé',
       subtitle = '(1990-2010)',
       caption = 'Fuente: Servicio de Registro Civil e Identificación a través del paquete de R "guaguas".') +
  theme_bw() + 
  scale_color_locuszoom() +
  transition_reveal(anio) +
  theme(legend.position= 'bottom',
        plot.caption = element_text(face = 'italic'),
        legend.text = element_text(size=12),
        legend.key.size = unit(1.5, 'cm'),
        plot.title = element_text(face = 'bold'),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  scale_y_continuous(breaks = scales::breaks_width(50))
  
anim_save('nombres_axe.gif')

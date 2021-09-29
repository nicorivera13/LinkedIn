###############################################
# Casos policiales DMCS - Región Metropolitana
# Autor: Nicolás Rivera G.
###############################################

# Paquetes ---------------------------
library(tidyverse)
library(ggplot2)
library(gganimate)
library(chilemapas)

# Directorio ---------------------------
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/casos_policiales')

# Mapa RM ---------------------------
RM <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  mutate(Comuna = as.numeric(codigo_comuna)) %>% 
  select(Comuna, geometry)

# Casos policiales ---------------------------
casos_policiales <- read_csv('CasosPolicialesDMCS_comunas.csv') 

casos_policiales <- casos_policiales %>% 
  right_join(RM, by='Comuna')

# Animación ---------------------------
anim <- ggplot(casos_policiales, aes(fill = Tasa_dmcs, 
                                     geometry = geometry)) + 
  geom_sf() +
  scale_fill_gradient(low = 'white',
                      high = 'red',
                      name = 'Tasa por 100 mil habs.',
                      breaks = c(2500,5000,7500,10000),
                      label = scales::comma) +
  theme_minimal(base_size = 13) +
  transition_manual(Anio) +
  labs(x = 'long',
       y = 'lat',
       title = 'Casos policiales DMCS - Región Metropolitana',
       subtitle = 'Año: {current_frame}',
       caption = 'Fuente: CEAD e INE.') +
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.caption = element_text(face = 'italic', size = 10),
        legend.title = element_text(size = 12)) 

time = max(casos_policiales$Anio)-min(casos_policiales$Anio)+1
animate(anim, duration = time)

anim_save('CasosPolicialesDMCS_RM.gif')

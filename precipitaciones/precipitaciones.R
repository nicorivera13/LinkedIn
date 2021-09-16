#############################
# Precipitaciones en Chile
# por Nicolás Rivera
#############################

# Packages ---------------------------
library(tidyverse)
library(ggplot2)
library(gganimate)
library(janitor)
library(maps)

# Working directory ---------------------------
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/precipitaciones')

# Importing data ---------------------------
stations <- read.table('cr2_prDaily_2020_stations.txt', sep = ',', fill = TRUE)

precipitaciones <- read.table('cr2_prDaily_2020.txt', sep = ',', fill = TRUE)

# Transforming data ---------------------------
stations <- stations %>%
  row_to_names(row_number = 1) %>% 
  select(codigo_estacion,nombre,latitud,longitud, institucion) %>% 
  filter(codigo_estacion != 'Rio Itata')

precipitaciones <- precipitaciones %>% 
  slice(-c(2,3,4,5,6,7,8)) %>% 
  select(1:880) %>% 
  row_to_names(row_number = 1) %>% 
  rename(date = codigo_estacion) %>% 
  gather(key = 'codigo_estacion', value = 'precip', -c('date')) %>% 
  mutate(code = ifelse(substr(codigo_estacion,1,1)==0,
                       substr(codigo_estacion,2,nchar(codigo_estacion)),
                       codigo_estacion),
         precip2 = ifelse(precip=='-9999', NA, as.numeric(precip)),
         year = substr(date,1,4)) %>% 
  select(-c('codigo_estacion','precip')) %>% 
  rename(codigo_estacion = code, 
         precip = precip2) %>% 
  filter(precip>=0 & !is.na(precip)) %>% 
  group_by(year, codigo_estacion) %>% 
  summarise(total_precip = sum(precip)) %>% 
  inner_join(stations, by = 'codigo_estacion')

precipitaciones$latitud <- as.numeric(levels(precipitaciones$latitud))[precipitaciones$latitud]
precipitaciones$longitud <- as.numeric(levels(precipitaciones$longitud))[precipitaciones$longitud]

quantile(precipitaciones$total_precip, prob=seq(0, 1, 0.05))
precipitaciones <- precipitaciones[(precipitaciones$total_precip>=0 & precipitaciones$total_precip<=2600) & (precipitaciones$year>=1970 & precipitaciones$year<=2019),]

# Map ---------------------------
anim <- ggplot(data=precipitaciones, 
            aes(x=longitud, y=latitud)) +
  geom_polygon(data = map_data('world'), 
               aes(x=long, y = lat, group = group), 
               fill='black', 
               alpha=0.5) +
  geom_point(aes(color=total_precip), size=2, alpha = 1) +
  scale_color_continuous(name = 'Milímetros',
                        low = 'white',
                        high = 'darkblue',
                        label = scales::comma) +
  coord_sf(xlim=c(-80,-63), ylim=c(-60,-15)) +
  borders(colour = 'black', size=0.25) +
  transition_manual(year) +
  labs(x = 'long',
       y = 'lat',
       title = 'Precipitación acumulada anual según estación',
       subtitle = 'Año: {current_frame}',
       caption = 'Fuente: Centro de Ciencia del Clima y la Resiliencia (CR)2.') +
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        plot.subtitle = element_text(size = 12)) 

time = max(as.numeric(precipitaciones$year))-min(as.numeric(precipitaciones$year))+1
animate(anim, duration = time/3)

anim_save('precipitaciones_chile.gif')

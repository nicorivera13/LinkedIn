######################################
# Votación diputados retiros 10% AFPs
# por Nicolás Rivera
######################################

# Se cargan paquetes
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)
library(hrbrthemes)

# Directorio
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/Retiros_afp')

# Datos
data <- read_excel('votacion_diputados.xlsx',
                   col_names = TRUE)

data$`Primer retiro`[data$`Primer retiro` == 4] <- NA
data$`Segundo retiro`[data$`Segundo retiro` == 4] <- NA
data$`Tercer retiro`[data$`Tercer retiro` == 4] <- NA
data$`Cuarto retiro`[data$`Cuarto retiro` == 4] <- NA

table(data$`Primer retiro`)
table(data$`Segundo retiro`)
table(data$`Tercer retiro`)
table(data$`Cuarto retiro`)

# Transformación dataset
data <- tidyr::gather(data, 
                      `Primer retiro`,
                      `Segundo retiro`, 
                      `Tercer retiro`,
                      `Cuarto retiro`,
                      key = 'Retiros', 
                      value = 'Votacion')  

# Clasificación de los diputados según bloque
oficialismo <- c('Evópoli','Ind-Evópoli','Ind-RN','PLR','PLRn 20​','PLRn 23​','PLRn 26​','RN','UDI')
oposicion <- c('Comunesn 5​','Comunesn 9​','CS','DC','FREVS','Ind-PPDn 17​','Ind-PR','PC','PCn 14​','PEV','PH','PL','PPD','PR','PS','RD','Unirn 4​')

data$Bloque <- c()
data$Bloque[data$Partido %in% oficialismo] <- 'Oficialismo'
data$Bloque[data$Partido %in% oposicion] <- 'Oposición'

data$Bloque[data$Nombre=='Florcita' & data$`Apellido paterno`=='Alarcón'] <- 'Oposición'
data$Bloque[data$Nombre=='René' & data$`Apellido paterno`=='Alinco'] <- 'Oposición'
data$Bloque[data$Nombre=='Sandra' & data$`Apellido paterno`=='Amar'] <- 'Oficialismo'
data$Bloque[data$Nombre=='Pepe' & data$`Apellido paterno`=='Auth'] <- 'Oposición'
data$Bloque[data$Nombre=='Bernardo' & data$`Apellido paterno`=='Berger'] <- 'Oficialismo'
data$Bloque[data$Nombre=='Álvaro' & data$`Apellido paterno`=='Carter'] <- 'Oficialismo'
data$Bloque[data$Nombre=='Natalia' & data$`Apellido paterno`=='Castillo'] <- 'Oposición'
data$Bloque[data$Nombre=='Marcelo' & data$`Apellido paterno`=='Díaz'] <- 'Oposición'
data$Bloque[data$Nombre=='Tomás' & data$`Apellido paterno`=='Hirsch'] <- 'Oposición'
data$Bloque[data$Nombre=='Carlos Abel' & data$`Apellido paterno`=='Jarpa'] <- 'Oposición'
data$Bloque[data$Nombre=='Harry' & data$`Apellido paterno`=='Jürgensen'] <- 'Oficialismo'
data$Bloque[data$Nombre=='Pablo' & data$`Apellido paterno`=='Lorenzini'] <- 'Oposición'
data$Bloque[data$Nombre=='Fernando' & data$`Apellido paterno`=='Meza'] <- 'Oposición'
data$Bloque[data$Nombre=='Patricio' & data$`Apellido paterno`=='Rosas'] <- 'Oposición'
data$Bloque[data$Nombre=='René' & data$`Apellido paterno`=='Saffirio'] <- 'Oposición'
data$Bloque[data$Nombre=='Virginia' & data$`Apellido paterno`=='Troncoso'] <- 'Oficialismo'
data$Bloque[data$Nombre=='Pedro' & data$`Apellido paterno`=='Velásquez'] <- 'Oposición'
data$Bloque[data$Nombre=='Pablo' & data$`Apellido paterno`=='Vidal'] <- 'Oposición'

sum(is.na(data$Bloque))

# Totales
data <- data %>% 
  select(Retiros,Bloque,Votacion) %>% 
  filter(!is.na(Votacion)) %>% 
  group_by(Retiros, Bloque, Votacion) %>% 
  summarise(Votos = n())

# Arreglos
data$Votacion[data$Votacion == 1] <- 'A favor'
data$Votacion[data$Votacion == 2] <- 'En contra'
data$Votacion[data$Votacion == 3] <- 'Abstención'

data$Retiros <- forcats::fct_recode(data$Retiros, 
'1er retiro' = 'Primer retiro', 
'2do retiro' = 'Segundo retiro', 
'3er retiro' = 'Tercer retiro', 
'4to retiro' = 'Cuarto retiro')

data$Retiros <- ordered(data$Retiros,
                        levels = c('1er retiro',
                                   '2do retiro',
                                   '3er retiro',
                                   '4to retiro'))

data$Votacion <- ordered(data$Votacion,
                         levels = c('A favor',
                                   'En contra',
                                   'Abstención'))

# Figura
ggplot(data, aes(fill=Bloque, y=Votos, x=Votacion)) + 
  geom_bar(position='dodge', stat='identity') +
  scale_fill_viridis(discrete = T, option = 'E') +
  geom_text(aes(label = Votos),
            position=position_dodge(width=0.9),
            vjust=-0.1,
            size = 3.5,
            colour = 'black') +
  ggtitle('Votación diputados retiros 10% AFPs') +
  facet_wrap(~Retiros) +
  theme_ipsum() +
  theme(legend.position='bottom',
        legend.title = element_blank(),
        plot.background = element_rect(fill = 'white')) +
  xlab('') +
  labs(caption = 'Fuente: Elaboración propia utilizando datos de la Cámara de Diputadas y Diputados.')

ggsave('votacion_diputados_retiros_afp.png')

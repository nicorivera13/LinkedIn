#############################
# Inflación en Chile
# por Nicolás Rivera
#############################

# Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(gganimate)

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/LinkedIn/inflation')

# Get the data ----------------------------
data <- read_excel("Cuadro_08092021092903.xlsx")

data <- data[-1,-c(3,4)]
colnames(data) <- c('Periodo','IPC')

data$IPC <- round(as.numeric(data$IPC),1)

data$Periodo <- gsub('T00:00:00.0000000','', data$Periodo)
data$Year <- as.numeric(substr(data$Periodo,1,4))
data$Month <- as.numeric(substr(data$Periodo,6,7))

data$Periodo <- as.Date(data$Periodo, format = '%Y-%m-%d')

data <- data %>%
  filter(Periodo>='2000-01-01') %>% 
  mutate(show_time = ifelse((Year == 2008 & Month %in% c(8,9,10)) 
                            | Year == 2021, 15, 1),
         reveal_time = cumsum(show_time))

# Animation ----------------------------
data %>%
  ggplot(aes(x=Periodo, y=IPC)) +
  geom_line(linetype = 'solid', 
            color = 'darkblue') +
  geom_point(color = 'darkblue') +
  labs(x='',
       y = '%',
       title = 'IPC, variación anual',
       subtitle = '(2000-2021)',
       caption = 'Fuente: Elaboración propia utilizando datos del INE y BCCh.') +
  transition_reveal(reveal_time) +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(face = 'italic',
                                    size = 10)) +
  scale_y_continuous(breaks = scales::breaks_width(2)) +
  scale_x_date(breaks = as.Date(c('2000-01-01',
                                  '2005-01-01',
                                  '2010-01-01',
                                  '2015-01-01',
                                  '2020-01-01')), 
               date_labels = "%Y")

anim_save('inflation_chile_2000-2021.gif')

##################################
# Population pyramid - Chile 
# by Nicolas Rivera
##################################

# Packages
library('tidyverse')
library('readxl')
#install.packages('wcde')
library('wcde')
#install.packages('lemon')
library('lemon')
#install.packages('gganimate')
library('gganimate')
#install.packages('gifski')
library('gifski')

# Working directory
setwd('C:/Users/HP/Desktop/MPP/presentacion/figuras')

# Getting data for Chile
cl_code <- get_wcde(country_name = 'Chile')
cl_code <- unique(cl_code$country_code)

chile <- get_wcde(indicator = 'epop', 
                  country_code = cl_code)

chile2 <- chile %>% 
  edu_group_sum(n = 4) %>%
  mutate(pop = ifelse(sex == "Male", -epop, epop),
         pop = pop/1e3)

chile2 <- chile2 %>%
  mutate(pop_max = ifelse(sex == "Male", -max(pop), max(pop)))

# Renaming categories of sex
chile2$sex <- forcats::fct_recode(chile2$sex,
                                  'Hombres' = 'Male',
                                  'Mujeres' = 'Female')
table(chile2$sex)

table(chile2$education)
# Population pyramid
ggplot(data = chile2, 
       mapping = aes(x = pop, 
                     y = age, 
                     fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, 
                    name = "Educación",
                    labels = c('Menos de 15 años','Sin educación','Básica','Media','Superior'),  
                    guide = guide_legend(reverse = FALSE)) +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin(b = 0, t = 0)),
        plot.caption = element_text(face = 'italic', size=8)) +
  transition_time(time = year) +
  labs(x = "Población (millones)", 
       y = "Edad", 
       title = 'Población en Chile {round(frame_time)}',
       caption = 'Fuente: Elaboración propia del CFA con datos del Wittgenstein Centre a través del paquete de R "wcde".')

anim_save('pop_chile.gif')

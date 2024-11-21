library(tidyverse)
library(readxl)
library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)
library(easystats)
library(ggplot2)
library(MASS)

Final_Project_Data <- read.csv("C:/Users/Owner/Desktop/Biol 3100/OStuart0608.github.io/IHME_GBD_2010_MORTALITY_AGE_SPECIFIC_BY_COUNTRY_1970_2010 (1).csv")
view(Final_Project_Data)

summary(Final_Project_Data)
names(Final_Project_Data)

Final_Project_Data %>%
  ggplot(mapping = aes(x = Country.Name, 
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
labs(title = 'Death Rates per Year',
     x = 'Year',
     y = 'Death Rate per 100000 ') + 
  theme(axis.text.x = element_text(angle=90, hjust = 1 ))

Final_Project_Data %>%
  ggplot(aes(x = Year, y = Death.Rate.Per.100.000))+
  geom_bar(stat = 'identity') +
  labs(title = 'Death Rates per Year',
       x = 'Year',
       y = 'Death Rate')










































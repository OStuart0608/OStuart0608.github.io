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




Final_Project_Data1 <- Final_Project_Data %>%
  filter(Country.Name == c('Australia', 'Brazil', 'Central Africa', 'Canada', 'China', 'France', 'United Kingdom', 'Japan', 'Mexico', 'United States')) %>%
  view()



Final_Project_Data2 <- aggregate(Number.of.Deaths ~ Country.Name, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data2) 





summary(Final_Project_Data1)
names(Final_Project_Data)

Final_Project_Data2 %>% 
  ggplot(mapping = aes(x = Country.Name, 
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
labs(title = 'Deaths per Country',
     x = 'Country',
     y = 'Death Max') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))


Final_Project_Data3<- aggregate(Number.of.Deaths ~ Year,data = Final_Project_Data1, FUN = max)
view(Final_Project_Data3)


as.character(Final_Project_Data3$Year)


Final_Project_Data3 %>%
  ggplot(mapping = aes(x = Year,
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Death Rates per Year',
       x = 'Year',
       y = 'Death Max') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))




Final_Project_Data3<- aggregate(Deathsr,data = Final_Project_Data1, FUN = max)
view(Final_Project_Data3)




































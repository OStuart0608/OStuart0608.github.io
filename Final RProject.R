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
  select(-Country.Code) %>%
  view()

## code for 1st plot on on my website




# Save to a CSV file
write.csv(Final_Project_Data2, "Final_Project_Data2(2).csv", row.names = FALSE)




# Clean the Number.of.Deaths column (remove commas and convert to numeric)
Final_Project_Data2 <- aggregate(Number.of.Deaths ~ Country.Name, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data2) 


sapply(Final_Project_Data2, class)

Final_Project_Data2$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data2$Number.of.Deaths))

# Create a bar plot

ggplot(Final_Project_Data2, aes(x = Country.Name, y = Number.of.Deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Deaths by Country", x = "Country", y = " Max Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Code for 2nd plot on website


Final_Project_Data4<- aggregate(Number.of.Deaths ~ Sex,data = Final_Project_Data1, FUN = max)
view(Final_Project_Data3)


as.character(Final_Project_Data3$Year)


Final_Project_Data3 %>%
  ggplot(mapping = aes(x = Sex,
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Death Rates per Year',
       x = 'Sex',
       y = 'Death Max') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))

Final_Project_Data3<- aggregate(Number.of.Deaths ~ Year, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data3)

head(Final_Project_Data3)

Final_Project_Data3$Year <- as.character(Final_Project_Data3$Year)
Final_Project_Data3$Number.of.Deaths <- as.character(Final_Project_Data3$Number.of.Deaths)
sapply(Final_Project_Data3, class)

view(Final_Project_Data3)
Final_Project_Data3 %>%
  ggplot(mapping = aes(x = Year,
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Death Rates per Year',
       x = 'Year',
       y = 'Death Max') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))


Final_Project_Data4 %>%
  ggplot(mapping = aes(x = Sex,
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Death Rates per Year',
       x = 'Sex',
       y = 'Death Max') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))

Final_Project_Data5 <- Final_Project_Data1


Final_Project_Data5$Year <- as.character(Final_Project_Data5$Year)
sapply(Final_Project_Data5, class)

view(Final_Project_Data5)


Final_Project_Data5 %>%
  ggplot(mapping = aes(x = Year,
                       y = Number.of.Deaths,
  )) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~Country.Name)
  labs(title = 'Death Rates per Year',
       x = 'Year',
       y = 'Number of Deaths') +
  theme(axis.text.x = element_text(angle=45, hjust = 1 ))


































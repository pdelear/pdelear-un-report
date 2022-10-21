library(tidyverse)

#load in data set
gapminder_data <- read_csv('data/gapminder_data.csv')

#summarizing our data
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% summarize(averageLifeExp = mean(lifeExp)) # "%>%" pipe operator, functions similarly to layers in ggplot

gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))

#filtering our data using filter()
gapminder_data %>% 
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

#find the earliest year in the dataset
gapminder_data %>% summarize(first_year = min(year))

#find the mean gdp per capita for that year
gapminder_data %>% 
  filter(year == 1952) %>%
  summarize(average_gdp = mean(gdpPercap))

#grouping data 
gapminder_data %>%
  group_by(year) %>%
  summarize(average = mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(average = mean(lifeExp), min = min(lifeExp))

#adding new columns with mutate
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap,
         popInMillions = pop / 1000000)

#subset columns (or change their order) with select()
gapminder_data %>%
  select(pop,year)

gapminder_data %>%
  select(continent, country)

#moving between long and wide data with pivot_wider() and pivot_longer()
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


#final data set for analysis
gapminder_data_2007 <- read_csv('data/gapminder_data.csv') %>% 
  filter(year == 2007 & continent == 'Americas') %>%
  select(-year, -continent)


#data cleaning 

read_csv('data/co2-un-data.csv', skip = 2,
         col_names = c('region','country','year',
                       'series','value','footnotes','source'))

read_csv('data/co2-un-data.csv', skip = 1) %>%
  rename(country = ...2)

read_csv('data/co2-un-data.csv', skip = 1) %>%
  rename_all(tolower)

##practicing select()
# we only want country year series and value
co2_emissions_dirty <- read_csv('data/co2-un-data.csv', skip = 2,
         col_names = c('region','country','year',
                       'series','value','footnotes','source'))

co2_emissions_dirty %>%
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emission',
                         'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  # number of observations per year
  count(year) 

co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emission',
                         'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)


# joining data frames

inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions, by = 'country')

co2_emissions <- read_csv('data/co2-un-data.csv',
                          skip = 2,
                          col_names = c('region', 'country', 'year', 'series', 'value',
                                        'footnotes', 'source')) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emission',
                         'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 'Bolivia (Plurin. State of)' = 'Bolivia',
                          'United States of America' = 'United States',
                          'Venezuela (Boliv. Rep. of)' = 'Venezuela'))

# a second anti join to check our work
anti_join(gapminder_data_2007,co2_emissions)

gapminder_data_2007 <- read_csv('data/gapminder_data.csv') %>% 
  filter(year == 2007 & continent == 'Americas') %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, 'Puerto Rico' = 'United States'))

anti_join(gapminder_data_2007, co2_emissions)

gapminder_data_2007 <- read_csv('data/gapminder_data.csv') %>% 
  filter(year == 2007 & continent == 'Americas') %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, 'Puerto Rico' = 'United States')) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop) / sum(pop), 
            gdpPercap = sum(gdpPercap * pop) / sum(pop))


gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = 'country')

gapminder_co2 %>% 
  mutate(region = if_else(country == 'Canada' |
                            country == 'United States' |
                            country == 'Mexico', 'north', 'south'))

write_csv(gapminder_co2, 'data/gapminder_co2.csv')

# Analyzing / plotting combined data

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
  geom_point() + 
  labs(x = 'GDP (per capita)', y = 'CO2 emitted (per capita)',
       title = "There is a strong association between a nation's GPD \nand the amount of CO2 it produces")
# fit a line to our data

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
  geom_point() + 
  labs(x = 'GDP (per capita)', y = 'CO2 emitted (per capita)',
       title = "There is a strong association between a nation's GPD \nand the amount of CO2 it produces") + 
  geom_smooth()

## fit a straight line to our data

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
  geom_point() + 
  labs(x = 'GDP (per capita)', y = 'CO2 emitted (per capita)',
       title = "There is a strong association between a nation's GPD \nand the amount of CO2 it produces") +
  geom_smooth(method = 'lm')









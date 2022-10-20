library(tidyverse)

gapminder_1997 <- read_csv('gapminder_1997.csv')

#this outputs the current date
Sys.Date()

#outputs the current working directory
getwd()

#Plotting!
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) + 
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do People In Wealthy Countries Live Longer?", size = 'Population (in millions)') + 
  geom_point() +
  scale_color_brewer(palette = 'Set1')


#Plotting for data exploration

gapminder_data <- read.csv('gapminder_data.csv')

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point()

str(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp, color = continent) +
  geom_boxplot()
#built in layers, so order of geoms is important
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = continent)) + 
  #color = 'blue', fill = 'pink' (can color by color name or variable)
  geom_jitter(alpha = 0.5)
  #alpha is transparency 
#alternative to choosing random colors: samples(colors(), size = 10) 


ggplot(gapminder_1997) + 
  aes(x = lifeExp) +
  geom_histogram() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))



#Facet

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point() +
  facet_wrap(vars(continent))

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point() +
  facet_grid(rows = vars(continent))

ggsave('awesome_plot.jpg', 
       width = 6, 
       height = 4)

violin_plot <- ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill = continent))

violin_plot + theme_bw()

violin_plot <- violin_plot + theme_bw()

ggsave(plot = violin_plot, 
       filename = 'awesome_violin_plot.jpg',
       width = 6,
       height = 4)

install.packages('gganimate', 'gifski')
library(gganimate)
library(gifski)

ggplot(data = gapminder_data) + 
  aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent) +
  geom_point()

staticHansPlot <- ggplot(data = gapminder_data) + 
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent) +
  geom_point(alpha = 0.5) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = 'GDP Per Capita', y = 'Life Expectancy', color = 'Continent', size = 'Population (in millions') +
  theme_classic()

staticHansPlot

animatedHansPlot <- staticHansPlot + 
  transition_states(year, transition_length = 1, state_length = 1) + 
  ggtitle('{closest_state}')
  
animatedHansPlot

anim_save('hansAnimatedPlot.gif',
          plot = animatedHansPlot,
          renderer = gifski_rendered())  
  
  
  
  


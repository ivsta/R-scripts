setwd('~/Desktop')

library(gapminder)
library(ggplot2)
library(gganimate)
theme_set(theme_bw())

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

gganimate(p, "output.gif")
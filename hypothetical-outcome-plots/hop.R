#--------------------------------------------------------------
# http://maxhumber.com/2017/02/03/hypothetical-outcomes.html
#--------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
# devtools::install_github("dgrtwo/gganimate")
# install.packages("cowplot")
library(gganimate)
# install image magick in terminal >> "brew install image magick"


set.seed(2016)

df <- tibble(
  `Blue Team` = round(rnorm(50, mean = 48, sd = 5)), 
  `Red Team` = `Blue Team` + round(rnorm(50, mean = -1, sd = 3))
) %>% 
  mutate(simulation = row_number()) %>% 
  gather(team, polling, -simulation) %>% 
  mutate(polling = ifelse(polling >= 100, 100, polling) / 100)

df %>% 
  ggplot(aes(x = team, y = polling)) +
  geom_boxplot()

df %>% 
  ggplot(aes(x = polling, fill = team)) + 
  geom_density(alpha = 1/2) + 
  scale_fill_manual(values = c("blue", "red"))

df %>% 
  group_by(team) %>% 
  summarise(
    mean = mean(polling), 
    low = quantile(polling, 0.025),
    high = quantile(polling, 0.975)) %>% 
  ggplot(aes(x = team, y = mean)) +
  geom_errorbar(aes(ymin = low, ymax = high))

df %>% 
  ggplot(aes(x = team, y = polling)) +
  geom_errorbar(aes(ymin = polling, ymax = polling))

p <- df %>% 
  ggplot(aes(x = team, y = polling, frame = simulation)) +
  geom_errorbar(aes(ymin = polling, ymax = polling))

gganimate(p, title_frame = FALSE, filename = '~/Desktop/output.gif') # Paste into browser to view


p <- df %>%
  ggplot(aes(x = team, y = polling)) +
  geom_errorbar(aes(
    ymin = polling, ymax = polling, 
    frame = simulation, cumulative = TRUE), 
    color = "grey80", alpha = 1/8) +
  geom_errorbar(aes(
    ymin = polling, ymax = polling, frame = simulation), 
    color = "#00a9e0") +
  scale_y_continuous(
    limits = c(0, 1), 
    labels = scales::percent_format()) +
  theme(panel.background = element_rect(fill = "#FFFFFF")) +
  labs(title = "", y = "Polling %", x = "")

gganimate(p, title_frame = FALSE, filename = '~/Desktop/output.gif') # Paste into browser to view
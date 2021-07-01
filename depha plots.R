install.packages("tidyverse")
install.packages("hexbin")
install.packages("ggplot2")
install.packages("lattice")
install.packages("gganimate")
install.packages("gifski")
install.packages("directlabels")
install.packages("transformr")
install.packages("png")
install.packages("grid")
install.packages("gapminder")
library(gapminder)
library(gganimate)
library(gifski)
str(gapminder)
head(gapminder)
summary(gapminder)
table(gapminder&continent)
aggregate(lifeExp ~ continent, gapminder, median)
plot(lifeExp ~ year, gapminder, subset = country == "Cambodia", type = "b")
plot(lifeExp ~ gdpPercap, gapminder, subset = year == 2007, log = "x")

if (require("dplyr")) {
  gapminder %>%
    filter(year == 2007) %>%
    group_by(continent) %>%
    summarise(lifeExp = median(lifeExp))
  
  # how many unique countries does the data contain, by continent?
  gapminder %>%
    group_by(continent) %>%
    summarize(n_obs = n(), n_countries = n_distinct(country))
  
  # by continent, which country experienced the sharpest 5-year drop in
  # life expectancy and what was the drop?
  gapminder %>%
    group_by(continent, country) %>%
    select(country, year, continent, lifeExp) %>%
    mutate(le_delta = lifeExp - lag(lifeExp)) %>%
    summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
    filter(min_rank(worst_le_delta) < 2) %>%
    arrange(worst_le_delta)
library(gapminder)
           h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
           h_dat <- droplevels(subset(gapminder, country %in% h_countries))
           h_dat$country <- with(h_dat, reorder(country, lifeExp, max))
           ggplot(h_dat, aes(x = year, y = lifeExp)) +
             geom_line(aes(color = country)) +
             scale_colour_manual(values = country_colors) +
             guides(color = guide_legend(reverse = TRUE))           



## ----global_options, include=FALSE--------------------------------------------
# R output pre blocks are styled by default to indicate output
knitr::opts_chunk$set(comment = NA)

library(rbokeh)
thm <- getOption("bokeh_theme")
thm$axis$axis_label_text_font_size <- "10pt"
options(bokeh_theme = thm)

## ---- message=FALSE-----------------------------------------------------------
library(trelliscopejs)
library(ggplot2)
library(gapminder)

str(gapminder)

## ----fig.width=11.8, fig.height=10--------------------------------------------
qplot(year, lifeExp, data = subset(gapminder, continent == "Europe")) +
  facet_wrap(~ country + continent) +
  theme_bw()

## ----fig.width=11.8, fig.height=10--------------------------------------------
qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_wrap(~ country + continent)

## ----fig.width=8, fig.align="center"------------------------------------------
qplot(class, cty, data = mpg, geom = c("boxplot", "jitter")) +
  ylim(7, 37) + theme_bw()

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(gapminder)

by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

by_country

## -----------------------------------------------------------------------------
country_model <- function(df)
  lm(lifeExp ~ year, data = df)

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country

## ----warning=FALSE------------------------------------------------------------
library(rbokeh)
library(trelliscopejs)

country_plot <- function(data, model) {
  figure(xlim = c(1948, 2011), ylim = c(10, 95), tools = NULL) %>%
    ly_points(year, lifeExp, data = data, hover = data) %>%
    ly_abline(model)
}

by_country <- by_country %>%
  mutate(data_plot = map2_plot(data, model, country_plot))

by_country

## ----warning=FALSE------------------------------------------------------------
by_country <- by_country %>%
  mutate(resid_mad = cog(
    map_dbl(model, ~ mad(resid(.x))),
    desc = "median absolute deviation of residuals"))


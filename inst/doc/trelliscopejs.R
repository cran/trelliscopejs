## ----global_options, include=FALSE--------------------------------------------
# R output pre blocks are styled by default to indicate output
knitr::opts_chunk$set(comment = NA)

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

by_country <- nest(gapminder, data = !one_of(c("country", "continent")))

by_country

## -----------------------------------------------------------------------------
country_model <- function(df)
  lm(lifeExp ~ year, data = df)

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country

## ----warning=FALSE------------------------------------------------------------
library(plotly)
library(trelliscopejs)

country_plot <- function(data, model) {
  plot_ly(data = data, x = ~year, y = ~lifeExp,
    type = "scatter", mode = "markers", name = "data") %>%
    add_trace(data = data, x = ~year, y = ~predict(model),
      mode = "lines", name = "lm") %>%
    layout(
      xaxis = list(range = c(1948, 2011)),
      yaxis = list(range = c(10, 95)),
      showlegend = FALSE)
}

by_country <- by_country %>%
  mutate(data_plot = map2_plot(data, model, country_plot))

by_country

## ----warning=FALSE------------------------------------------------------------
by_country <- by_country %>%
  mutate(resid_mad = cog(
    map_dbl(model, ~ mad(resid(.x))),
    desc = "median absolute deviation of residuals"))


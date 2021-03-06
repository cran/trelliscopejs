#' Panels Wrapper Function
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @details See \code{\link[purrr]{map}}
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' library(plotly)
#' ggplot2::mpg %>%
#'   nest(data = !one_of(c("manufacturer", "class"))) %>%
#'   mutate(panel = map_plot(data, function(x) {
#'     plot_ly(data = x, x = ~hwy, y = ~cty,
#'       type = "scatter", mode = "markers")
#'   })) %>%
#'   trelliscope(name = "city_vs_highway_mpg")
#' }
#' @export
panels <- function(.x, .f, ...) {
  warning("Note: The 'panels' function will be deprecated in the next release of this ",
    "package. Please use the equivalent 'map_plot' instead.")
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

#' Apply a function to each element of a vector and return a vector of plots
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @details See \code{\link[purrr]{map}}
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' library(plotly)
#' library(gapminder)
#'
#' # nest gapminder data by country
#' by_country <- gapminder %>%
#'   nest(data = !one_of(c("country", "continent")))
#' 
#' # add in a plot column with map_plot
#' by_country <- by_country %>% mutate(
#'   panel = map_plot(data, function(x) {
#'     plot_ly(data = x, x = ~year, y = ~lifeExp,
#'       type = "scatter", mode = "markers") %>%
#'       layout(
#'         xaxis = list(range = c(1948, 2011)),
#'         yaxis = list(range = c(10, 95)))
#'   }))
#' 
#' # plot it
#' by_country %>%
#'   trelliscope("gapminder", nrow = 2, ncol = 7, width = 300)
#' 
#' # example using mpg data
#' ggplot2::mpg %>%
#'   nest(data = !one_of(c("manufacturer", "class"))) %>%
#'   mutate(panel = map_plot(data, function(x) {
#'     plot_ly(data = x, x = ~hwy, y = ~cty,
#'       type = "scatter", mode = "markers")
#'   })) %>%
#'   trelliscope(name = "city_vs_highway_mpg")
#' }
#' @export
map_plot <- function(.x, .f, ...) {
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

#' Map over multiple inputs simultaneously and return a vector of plots
#'
#' @param .x,.y Vectors of the same length. A vector of length 1 will be recycled.
#' @param .f A function, formula, or atomic vector (see \code{\link[purrr]{map2}} for details)
#' @param ... additional arguments passed on to .f.
#' @param .l A list of lists. The length of .l determines the number of arguments that .f will be called with. List names will be used if present.
#' @details See \code{\link[purrr]{map2}}
#' @examples
#' \donttest{
#' library(tidyr)
#' library(purrr)
#' library(plotly)
#' library(dplyr)
#'
#' iris %>%
#'   nest(data = -Species) %>%
#'   mutate(
#'     mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
#'     panel = map2_plot(data, mod, function(data, mod) {
#'       plot_ly(data = data, x = ~Sepal.Width, y = ~Sepal.Length,
#'         type = "scatter", mode = "markers", name = "data") %>%
#'         add_trace(data = data, x = ~Sepal.Width, y = ~predict(mod),
#'           mode = "lines", name = "lm")
#'     })) %>%
#'   trelliscope(name = "iris")
#' }
#' @export
map2_plot <- function(.x, .y, .f, ...) {
  structure(
    purrr::map2(.x, .y, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

#' @export
#' @rdname map2_plot
pmap_plot <- function(.l, .f, ...) {
  structure(
    purrr::pmap(.l, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

# #' Apply a function to each row of a data frame and return a data frame with new column of plots
# #'
# #' @param .d A data frame.
# #' @param ..f A function to apply to each row. It should return a valid panel object (such as a ggplot2 / lattice / htmlwidget object).
# #' @param .to Name of output column (defaults to "panel").
# #' @details See \code{\link[purrrlyr]{by_row}}
# #' @examples
# #' \dontrun{
# #' library(tidyr)
# #' library(purrr)
# #' iris %>%
# #'   nest(-Species) %>%
# #'   mutate(mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x))) %>%
# #'   by_row_panel(function(x) {
# #'     figure(xlab = "Sepal.Width", ylab = "Sepal.Length") %>%
# #'       ly_points(x$data[[1]]$Sepal.Width, x$data[[1]]$Sepal.Length) %>%
# #'       ly_abline(x$mod[[1]])
# #'   }) %>%
# #'   trelliscope(name = "iris")
# #' }
# #' @export
# by_row_plot <- function(.d, ..f, .to = "panel") {
#   res <- purrrlyr::by_row(.d = .d, ..f = ..f, .to = .to)
#   class(res[[.to]]) <- c("trelliscope_panels", "list")
#   res
# }

#' Cogs Wrapper Function
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @export
#' @details See \code{\link[purrr]{map}}
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' library(plotly)
#' ggplot2::mpg %>%
#'   nest(data = !one_of(c("manufacturer", "class"))) %>%
#'   mutate(
#'     additional_cogs = map_cog(data, function(x) {
#'       tibble(
#'         max_city_mpg = cog(max(x$cty), desc = "Max city mpg"),
#'         min_city_mpg = cog(min(x$cty), desc = "Min city mpg"))
#'     }),
#'     panel = map_plot(data, function(x) {
#'       plot_ly(data = x, x = ~cty, y = ~hwy,
#'         type = "scatter", mode = "markers")
#'     })
#'   ) %>%
#'   trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
#' }
cogs <- function(.x, .f, ...) {
  warning("Note: The 'cogs' function will be deprecated in the next release of this ",
    "package. Please use the equivalent 'map_cog' instead.")
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_cogs", "list")
  )
}

#' Apply a function to each element of a vector and return a vector of cognostics data frames
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @details See \code{\link[purrr]{map}}
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' library(plotly)
#' ggplot2::mpg %>%
#'   nest(data = !one_of(c("manufacturer", "class"))) %>%
#'   mutate(
#'     cog = map_cog(data, function(x) tibble(mean_hwy = mean(x$hwy))),
#'     panel = map_plot(data, function(x) {
#'       plot_ly(data = x, x = ~cty, y = ~hwy,
#'         type = "scatter", mode = "markers")
#'     })
#'   ) %>%
#'   trelliscope(name = "city_vs_highway_mpg")
#' }
#' @export
map_cog <- function(.x, .f, ...) {
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_cogs", "list")
  )
}

#' Map over multiple inputs simultaneously and return a vector of cognostics data frames
#'
#' @param .x,.y Vectors of the same length. A vector of length 1 will be recycled.
#' @param .f A function, formula, or atomic vector (see \code{\link[purrr]{map2}} for details)
#' @param ... additional arguments passed on to .f.
#' @param .l A list of lists. The length of .l determines the number of arguments that .f will be called with. List names will be used if present.
#' @details See \code{\link[purrr]{map2}}
#' @examples
#' \donttest{
#' library(tidyr)
#' library(purrr)
#' library(plotly)
#' library(dplyr)
#'
#' iris %>%
#'   nest(data = -Species) %>%
#'   mutate(
#'     mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
#'     cogs = map2_cog(data, mod, function(data, mod) {
#'       tibble(max_sl = max(data$Sepal.Length), slope = coef(mod)[2])
#'     }),
#'     panel = map2_plot(data, mod, function(data, mod) {
#'       plot_ly(data = data, x = ~Sepal.Width, y = ~Sepal.Length,
#'         type = "scatter", mode = "markers", name = "data") %>%
#'         add_trace(data = data, x = ~Sepal.Width, y = ~predict(mod),
#'           mode = "lines", name = "lm")
#'     })) %>%
#'   trelliscope(name = "iris")
#' }
#' @export
map2_cog <- function(.x, .y, .f, ...) {
  structure(
    purrr::map2(.x, .y, .f, ...),
    class = c("trelliscope_cogs", "list")
  )
}

#' @export
#' @rdname map2_cog
pmap_cog <- function(.l, .f, ...) {
  structure(
    purrr::pmap(.l, .f, ...),
    class = c("trelliscope_cogs", "list")
  )
}

#' @export
`[.trelliscope_panels` <- function(x, i, j, ..., drop = TRUE) {
  cls <- class(x)
  x <- NextMethod()
  structure(x, class = cls)
}

#' @export
`[.trelliscope_cogs` <- function(x, i, j, ..., drop = TRUE) {
  cls <- class(x)
  x <- NextMethod()
  structure(x, class = cls)
}

#' @export
`[.panel_promise` <- function(x, i, j, ..., drop = TRUE) {
  cls <- class(x)
  x <- NextMethod()
  structure(x, class = cls)
}


# #' Apply a function to each row of a data frame and return a data frame with new column of cognostics
# #'
# #' @param .d A data frame.
# #' @param ..f A function to apply to each row. It should return a single-row data frame of cognostics
# #' @param .to Name of output column (defaults to "cogs" or if "cogs" exists, "cogs1", etc.).
# #' @details See \code{\link[purrrlyr]{by_row}}
# #' @examples
# #' \dontrun{
# #' library(tidyr)
# #' library(purrr)
# #' iris %>%
# #'   nest(-Species) %>%
# #'   mutate(mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x))) %>%
# #'   panels_by_row(function(x) {
# #'     figure(xlab = "Sepal.Width", ylab = "Sepal.Length") %>%
# #'       ly_points(x$data[[1]]$Sepal.Width, x$data[[1]]$Sepal.Length) %>%
# #'       ly_abline(x$mod[[1]])
# #'   }) %>%
# #'   trelliscope(name = "iris")
# #' }
# #' @export
# by_row_cog <- function(.d, ..f, .to = NULL) {
#   if (is.null(.to)) {
#     nms <- names(.d)
#     nms <- nms[grepl("^cogs[0-9]*$", nms)]
#     if (length(nms) > 0) {
#       nums <- as.integer(gsub("cogs", "", nms))
#       nums[is.na(nums)] <- 0
#       .to <- paste0("cogs", max(nums) + 1)
#     } else {
#       .to <- "cogs"
#     }
#   }
#   res <- purrrlyr::by_row(.d = .d, ..f = ..f, .to = .to)
#   class(res[[.to]]) <- c("trelliscope_cogs", "list")
#   res
# }

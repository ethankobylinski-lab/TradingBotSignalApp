suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(zoo)
})

source(file.path("data", "cache.R"))

load_price_history <- function(symbols, start = Sys.Date() - 365, end = Sys.Date(),
                               max_age = as.difftime(4, units = "hours")) {
  if (length(symbols) == 0) {
    return(tibble(symbol = character(), date = as.Date(character()), adjusted = numeric()))
  }
  start <- as.Date(start)
  end <- as.Date(end)

  purrr::map_dfr(symbols, function(sym) {
    key <- paste0("prices_", sym)
    data <- with_cache(key, max_age, {
      xt <- tryCatch({
        suppressWarnings(
          quantmod::getSymbols(sym, from = "1900-01-01", auto.assign = FALSE, warnings = FALSE)
        )
      }, error = function(e) NULL)
      if (is.null(xt)) {
        tibble(symbol = sym, date = as.Date(character()), adjusted = numeric())
      } else {
        adj <- quantmod::Ad(xt)
        tibble(
          symbol = sym,
          date = as.Date(zoo::index(adj)),
          adjusted = as.numeric(adj)
        )
      }
    })
    dplyr::filter(data, date >= start, date <= end)
  }) %>%
    arrange(symbol, date)
}

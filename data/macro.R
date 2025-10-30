suppressPackageStartupMessages({
  library(quantmod)
  library(tibble)
  library(dplyr)
  library(purrr)
  library(zoo)
})

source(file.path("data", "cache.R"))

load_macro <- function(series = c("DGS10", "DTWEXM"), start = Sys.Date() - 365, end = Sys.Date(),
                       max_age = as.difftime(12, units = "hours")) {
  if (length(series) == 0) {
    return(tibble(symbol = character(), date = as.Date(character()), value = numeric()))
  }
  start <- as.Date(start)
  end <- as.Date(end)

  purrr::map_dfr(series, function(sym) {
    key <- paste0("macro_", sym)
    data <- with_cache(key, max_age, {
      xt <- tryCatch({
        suppressWarnings(quantmod::getSymbols(sym, src = "FRED", auto.assign = FALSE))
      }, error = function(e) NULL)
      if (is.null(xt)) {
        tibble(symbol = sym, date = as.Date(character()), value = numeric())
      } else {
        tibble(
          symbol = sym,
          date = as.Date(zoo::index(xt)),
          value = as.numeric(xt[, 1])
        )
      }
    })
    dplyr::filter(data, date >= start, date <= end)
  }) %>%
    arrange(symbol, date)
}

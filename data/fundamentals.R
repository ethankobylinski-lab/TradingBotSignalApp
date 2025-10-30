suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(tibble)
  library(purrr)
})

source(file.path("data", "cache.R"))

.parse_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  as.numeric(gsub(",", "", x))
}

.parse_market_cap <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  x <- trimws(toupper(as.character(x)))
  mult <- 1
  if (grepl("T$", x)) mult <- 1e12
  else if (grepl("B$", x)) mult <- 1e9
  else if (grepl("M$", x)) mult <- 1e6
  else if (grepl("K$", x)) mult <- 1e3
  as.numeric(gsub("[^0-9.]+", "", x)) * mult
}

.safe_extract <- function(row, field, parser = .parse_numeric) {
  if (!field %in% colnames(row)) return(NA_real_)
  parser(row[[field]])
}

load_fundamentals <- function(symbols,
                              fields = c("Price/Earnings", "EPS (ttm)", "PEG Ratio", "Book Value", "Dividend Yield", "Market Cap"),
                              max_age = as.difftime(1, units = "days")) {
  if (length(symbols) == 0) {
    return(tibble(symbol = character(), date = as.Date(character())))
  }

  yahoo_fields <- quantmod::yahooQF(fields)
  purrr::map_dfr(symbols, function(sym) {
    key <- paste0("fundamentals_", sym)
    with_cache(key, max_age, {
      quotes <- tryCatch({
        quantmod::getQuote(sym, what = yahoo_fields)
      }, error = function(e) NULL)
      if (is.null(quotes) || nrow(quotes) == 0) {
        tibble(symbol = sym, date = as.Date(character()))
      } else {
        row <- quotes[1, , drop = FALSE]
        tibble(
          symbol = sym,
          date = as.Date(Sys.Date()),
          pe_ratio = .safe_extract(row, "Price/Earnings"),
          eps = .safe_extract(row, "EPS (ttm)"),
          peg_ratio = .safe_extract(row, "PEG Ratio"),
          book_value = .safe_extract(row, "Book Value"),
          dividend_yield = .safe_extract(row, "Dividend Yield"),
          market_cap = .safe_extract(row, "Market Cap", .parse_market_cap)
        )
      }
    })
  })
}

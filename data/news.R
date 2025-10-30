suppressPackageStartupMessages({
  library(xml2)
  library(tibble)
  library(dplyr)
  library(purrr)
  library(stringr)
})

source(file.path("data", "cache.R"))

.parse_pub_date <- function(x) {
  tryCatch({
    as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")
  }, error = function(e) NA)
}

.estimate_sentiment <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_real_)
  text <- tolower(text)
  pos <- sum(stringr::str_count(text, "\bgain\b|\brug\b|\bbeat\b|\brise\b"))
  neg <- sum(stringr::str_count(text, "\bloss\b|\bdown\b|\bmiss\b|\bcut\b"))
  pos - neg
}

load_news <- function(symbols, start = Sys.Date() - 30, end = Sys.Date(),
                      max_age = as.difftime(2, units = "hours")) {
  if (length(symbols) == 0) {
    return(tibble(symbol = character(), date = as.Date(character()), headline = character(),
                  link = character(), sentiment = numeric()))
  }
  start <- as.Date(start)
  end <- as.Date(end)

  purrr::map_dfr(symbols, function(sym) {
    key <- paste0("news_", sym)
    with_cache(key, max_age, {
      url <- sprintf("https://feeds.finance.yahoo.com/rss/2.0/headline?s=%s&region=US&lang=en-US", sym)
      doc <- tryCatch(xml2::read_xml(url), error = function(e) NULL)
      if (is.null(doc)) {
        tibble(symbol = sym, date = as.Date(character()), headline = character(),
               link = character(), sentiment = numeric())
      } else {
        items <- xml2::xml_find_all(doc, "//item")
        purrr::map_dfr(items, function(item) {
          pub_date <- xml2::xml_text(xml2::xml_find_first(item, "pubDate"))
          ts <- .parse_pub_date(pub_date)
          if (is.na(ts)) {
            tibble(symbol = sym, date = as.Date(NA), headline = NA_character_,
                   link = NA_character_, sentiment = NA_real_)
          } else {
            headline <- xml2::xml_text(xml2::xml_find_first(item, "title"))
            tibble(
              symbol = sym,
              date = as.Date(ts),
              headline = headline,
              link = xml2::xml_text(xml2::xml_find_first(item, "link")),
              sentiment = .estimate_sentiment(headline)
            )
          }
        })
      }
    }) %>%
      dplyr::filter(date >= start, date <= end)
  }) %>%
    distinct(symbol, date, headline, link, sentiment)
}

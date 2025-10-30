#' Simulate multi-asset market data with engineered factors
#'
#' @param symbols Character vector of tickers.
#' @param n Number of observations per symbol.
#' @param seed Random seed for reproducibility.
#' @return Tibble with prices, returns, and factor features.
#' @export
simulate_market_data <- function(symbols = c("AAA", "BBB", "CCC", "DDD"),
                                 n = 750,
                                 seed = 123) {
  set.seed(seed)
  dates <- seq.Date(from = Sys.Date() - n + 1, by = "day", length.out = n)
  base <- purrr::map_dfr(symbols, function(sym) {
    shocks <- rnorm(n, mean = 0.0005, sd = 0.02)
    price <- 100 * exp(cumsum(shocks))
    tibble::tibble(date = dates, symbol = sym, price = price)
  })

  features <- base |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      ret_1d = dplyr::lead(log(price)) - log(price),
      momentum_21 = zoo::rollapply(ret_1d, width = 21, FUN = sum, align = "right", fill = NA_real_),
      momentum_63 = zoo::rollapply(ret_1d, width = 63, FUN = sum, align = "right", fill = NA_real_),
      volatility_21 = sqrt(252) * zoo::rollapply(ret_1d, width = 21, FUN = stats::sd, align = "right", fill = NA_real_),
      quality_score = as.numeric(stats::filter(rnorm(dplyr::n(), 0, 1), rep(1/5, 5), sides = 1)),
      value_score = as.numeric(stats::filter(rnorm(dplyr::n(), 0, 1), rep(1/30, 30), sides = 1))
    ) |>
    dplyr::ungroup()

  benchmark <- features |>
    dplyr::group_by(date) |>
    dplyr::summarise(benchmark_price = mean(price), .groups = "drop")

  dplyr::left_join(features, benchmark, by = "date") |>
    tibble::as_tibble()
}

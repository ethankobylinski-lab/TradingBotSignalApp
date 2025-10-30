#' Rule based strategies
#'
#' Simple moving-average crossover strategy producing binary weights.
#'
#' @param data Input tibble with columns `date`, `symbol`, and `price`.
#' @param params List controlling lookbacks and weights.
#' @return Tibble of signals with columns `date`, `symbol`, `weight`, and `grade`.
#' @export
rule_based_ma_cross <- function(data, params = list()) {
  checkmate::assert_data_frame(data)
  params <- modifyList(list(short = 20L, long = 100L, max_weight = 1), params)
  if (params$short >= params$long) {
    stop("`short` lookback must be less than `long` lookback.")
  }
  signals <- data |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      ma_short = zoo::rollmean(price, k = params$short, fill = NA_real_, align = "right"),
      ma_long = zoo::rollmean(price, k = params$long, fill = NA_real_, align = "right"),
      grade = ma_short / ma_long - 1,
      weight = dplyr::if_else(ma_short > ma_long, params$max_weight, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, symbol, weight, grade)
  signals$weight[is.na(signals$weight)] <- 0
  signals$grade[is.na(signals$grade)] <- 0
  tibble::as_tibble(signals)
}

#' Price breakout strategy that allocates when price exceeds trailing high.
#'
#' @param data Input tibble with columns `date`, `symbol`, and `price`.
#' @param params List containing `lookback` (default 120) and `max_weight` (default 1).
#' @export
rule_based_breakout <- function(data, params = list()) {
  checkmate::assert_data_frame(data)
  params <- modifyList(list(lookback = 120L, max_weight = 1), params)
  lookback <- as.integer(params$lookback)
  if (lookback <= 1) stop("`lookback` must be greater than 1.")
  signals <- data |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      trailing_high = zoo::rollapply(price, width = lookback, align = "right", fill = NA_real_, FUN = max, na.rm = TRUE),
      grade = price / trailing_high - 1,
      weight = dplyr::if_else(price >= trailing_high, params$max_weight, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, symbol, weight, grade)
  signals$weight[is.na(signals$weight)] <- 0
  signals$grade[is.na(signals$grade)] <- 0
  tibble::as_tibble(signals)
}

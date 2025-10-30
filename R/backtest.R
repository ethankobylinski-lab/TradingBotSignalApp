#' Run a portfolio backtest using a strategy function
#'
#' @param strategy A character key resolving to a strategy function or a function itself.
#'   Strategy functions must accept `data` and `params` arguments and return a tibble with
#'   columns `date`, `symbol`, `weight` (target weights), and optional `grade` scores.
#' @param data A tibble containing at minimum `date`, `symbol`, and `price` columns. Optionally
#'   may contain a `benchmark` column with benchmark prices for comparison.
#' @param params A list of strategy specific parameters.
#' @param benchmark Optional tibble with `date` and `benchmark` columns containing benchmark
#'   returns or prices. If omitted and `data` contains a `benchmark_price` column, it will be used.
#' @param initial_capital Starting capital for the backtest.
#' @param cost_bps Transaction cost in basis points applied on traded notional each rebalance.
#' @return A list with components `trades`, `pnl`, `diagnostics`, and `benchmark`.
#' @export
run_backtest <- function(strategy,
                         data,
                         params = list(),
                         benchmark = NULL,
                         initial_capital = 100000,
                         cost_bps = 0) {
  checkmate::assert_data_frame(data)
  req_cols <- c("date", "symbol", "price")
  if (!all(req_cols %in% names(data))) {
    stop("`data` must contain columns: ", paste(req_cols, collapse = ", "))
  }
  if (!inherits(data$date, "Date")) {
    data$date <- as.Date(data$date)
  }
  data <- dplyr::arrange(data, date, symbol)
  strategy_fn <- resolve_strategy(strategy)
  signals <- strategy_fn(data = data, params = params)
  if (!nrow(signals)) {
    stop("Strategy returned no signals.")
  }
  signals <- validate_signals(signals)

  merged <- dplyr::left_join(data, signals, by = c("date", "symbol"))
  merged <- dplyr::group_by(merged, symbol)
  merged <- dplyr::arrange(merged, date, .by_group = TRUE)
  merged <- dplyr::mutate(
    merged,
    ret = dplyr::if_else(dplyr::row_number() == 1,
                         0,
                         log(price / dplyr::lag(price)))
  )
  merged <- dplyr::ungroup(merged)
  merged$weight[is.na(merged$weight)] <- 0
  merged$grade_percentile <- compute_grade_percentile(merged)

  trades <- merged |>
    dplyr::group_by(symbol) |>
    dplyr::mutate(
      prev_weight = dplyr::lag(weight, default = 0),
      trade = weight - prev_weight
    ) |>
    dplyr::ungroup()

  daily <- trades |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      portfolio_ret = sum(weight * ret, na.rm = TRUE),
      gross_turnover = sum(abs(trade), na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  trade_cost <- cost_bps / 10000
  daily <- dplyr::mutate(
    daily,
    transaction_cost = gross_turnover * trade_cost,
    net_ret = portfolio_ret - transaction_cost,
    equity = accumulate_returns(net_ret, initial_capital)
  )

  bm_returns <- prepare_benchmark(data, benchmark)
  diagnostics <- evaluate_backtest(daily, bm_returns, trades)

  list(
    trades = trades,
    pnl = daily,
    diagnostics = diagnostics,
    benchmark = bm_returns
  )
}

resolve_strategy <- function(strategy) {
  if (is.function(strategy)) {
    return(strategy)
  }
  strategy_map <- list(
    rule_ma_cross = rule_based_ma_cross,
    rule_breakout = rule_based_breakout,
    factor_score = factor_rank_score,
    elastic_net = ml_elastic_net,
    random_forest = ml_random_forest,
    xgboost = ml_xgboost
  )
  if (!is.character(strategy) || !strategy %in% names(strategy_map)) {
    stop("Unknown strategy key. Provide a function or one of: ", paste(names(strategy_map), collapse = ", "))
  }
  strategy_map[[strategy]]
}

validate_signals <- function(signals) {
  checkmate::assert_data_frame(signals)
  req <- c("date", "symbol", "weight")
  if (!all(req %in% names(signals))) {
    stop("Strategy signals must include columns: ", paste(req, collapse = ", "))
  }
  if (!inherits(signals$date, "Date")) {
    signals$date <- as.Date(signals$date)
  }
  signals$grade <- signals$grade %||% NA_real_
  dplyr::arrange(signals, .data$date, .data$symbol)
}

accumulate_returns <- function(returns, initial_capital) {
  equity <- numeric(length(returns))
  capital <- initial_capital
  for (i in seq_along(returns)) {
    capital <- capital * exp(returns[i])
    equity[i] <- capital
  }
  equity
}

compute_grade_percentile <- function(merged) {
  if (!"grade" %in% names(merged)) {
    merged$grade <- NA_real_
  }
  merged |>
    dplyr::group_by(date) |>
    dplyr::mutate(
      grade_percentile = if (all(is.na(grade))) {
        NA_real_
      } else {
        dplyr::percent_rank(grade)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(grade_percentile)
}

prepare_benchmark <- function(data, benchmark) {
  if (!is.null(benchmark)) {
    checkmate::assert_data_frame(benchmark)
    if (all(c("date", "benchmark") %in% names(benchmark))) {
      return(dplyr::arrange(benchmark, .data$date))
    }
  }
  if ("benchmark_price" %in% names(data)) {
    bench <- data |>
      dplyr::distinct(.data$date, .data$benchmark_price) |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(benchmark = dplyr::if_else(dplyr::row_number() == 1,
                                               0,
                                               log(.data$benchmark_price / dplyr::lag(.data$benchmark_price)))) |>
      dplyr::select(.data$date, .data$benchmark)
    return(bench)
  }
  tibble::tibble(date = sort(unique(data$date)), benchmark = NA_real_)
}

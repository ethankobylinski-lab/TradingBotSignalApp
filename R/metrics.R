#' Evaluate strategy performance metrics
#'
#' @param pnl Tibble with columns `date` and `net_ret` at minimum.
#' @param benchmark Tibble with `date` and `benchmark` returns (log or simple) matching `pnl$date`.
#' @param trades Tibble with executed trades from `run_backtest`.
#' @return A list containing a named vector of metrics and supporting tibbles.
#' @export
evaluate_backtest <- function(pnl, benchmark, trades) {
  checkmate::assert_data_frame(pnl)
  checkmate::assert_subset(c("date", "net_ret"), names(pnl))

  pnl <- dplyr::arrange(pnl, date)
  returns <- pnl$net_ret
  bench <- align_benchmark(pnl$date, benchmark)

  metrics <- list(
    entry_grade_percentile = calc_entry_grade_percentile(trades),
    cagr = calc_cagr(returns),
    max_drawdown = calc_max_drawdown(returns),
    upside_capture = calc_capture_ratio(returns, bench, type = "upside"),
    downside_capture = calc_capture_ratio(returns, bench, type = "downside"),
    sp500_tracking_error = calc_tracking_error(returns, bench)
  )

  list(
    metrics = metrics,
    metrics_table = tibble::tibble(metric = names(metrics), value = unlist(metrics)),
    returns = tibble::tibble(date = pnl$date, strategy = returns, benchmark = bench)
  )
}

calc_entry_grade_percentile <- function(trades) {
  if (is.null(trades) || !nrow(trades) || !"grade_percentile" %in% names(trades)) {
    return(NA_real_)
  }
  entries <- trades |>
    dplyr::filter(trade > 0, !is.na(grade_percentile))
  if (!nrow(entries)) {
    return(NA_real_)
  }
  mean(entries$grade_percentile, na.rm = TRUE)
}

calc_cagr <- function(returns, periods_per_year = 252) {
  valid <- returns[is.finite(returns)]
  if (!length(valid)) return(NA_real_)
  total <- sum(valid)
  exp(total)^(periods_per_year / length(valid)) - 1
}

calc_max_drawdown <- function(returns) {
  if (!length(returns)) return(NA_real_)
  equity <- exp(cumsum(replace_na_num(returns)))
  peak <- cummax(equity)
  drawdown <- equity / peak - 1
  min(drawdown, na.rm = TRUE)
}

calc_capture_ratio <- function(strategy, benchmark, type = c("upside", "downside")) {
  type <- match.arg(type)
  if (is.null(benchmark) || !length(benchmark)) return(NA_real_)
  valid <- !is.na(strategy) & !is.na(benchmark)
  if (!any(valid)) return(NA_real_)
  bench <- benchmark[valid]
  strat <- strategy[valid]
  capture_set <- if (type == "upside") bench > 0 else bench < 0
  if (!any(capture_set)) return(NA_real_)
  strat_avg <- mean(strat[capture_set], na.rm = TRUE)
  bench_avg <- mean(bench[capture_set], na.rm = TRUE)
  if (is.na(bench_avg) || bench_avg == 0) return(NA_real_)
  strat_avg / bench_avg
}

calc_tracking_error <- function(strategy, benchmark, periods_per_year = 252) {
  if (is.null(benchmark) || !length(benchmark)) return(NA_real_)
  valid <- !is.na(strategy) & !is.na(benchmark)
  if (!any(valid)) return(NA_real_)
  aligned <- strategy[valid] - benchmark[valid]
  sqrt(periods_per_year) * stats::sd(aligned, na.rm = TRUE)
}

align_benchmark <- function(dates, benchmark) {
  if (is.null(benchmark) || !nrow(benchmark)) {
    return(rep(NA_real_, length(dates)))
  }
  checkmate::assert_data_frame(benchmark)
  if (!all(c("date", "benchmark") %in% names(benchmark))) {
    stop("Benchmark must contain `date` and `benchmark` columns.")
  }
  benchmark <- dplyr::mutate(benchmark, date = as.Date(date))
  joined <- dplyr::right_join(benchmark, tibble::tibble(date = dates), by = "date")
  joined <- dplyr::arrange(joined, date)
  joined$benchmark
}

replace_na_num <- function(x, value = 0) {
  x[is.na(x)] <- value
  x
}

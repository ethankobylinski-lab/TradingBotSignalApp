library(targets)

tar_option_set(
  packages = c(
    "dplyr", "tibble", "purrr", "tidyr", "zoo", "checkmate",
    "recipes", "parsnip", "workflows", "rsample",
    "glmnet", "ranger", "xgboost"
  ),
  format = "rds"
)

tar_source("R")

list(
  tar_target(
    market_data,
    simulate_market_data(),
    format = "rds"
  ),
  tar_target(
    rule_ma_cross_backtest,
    run_backtest("rule_ma_cross", market_data, params = list(short = 30, long = 120, max_weight = 1)),
    format = "rds"
  ),
  tar_target(
    rule_breakout_backtest,
    run_backtest("rule_breakout", market_data, params = list(lookback = 90, max_weight = 1)),
    format = "rds"
  ),
  tar_target(
    factor_score_backtest,
    run_backtest("factor_score", market_data, params = list(top_n = 5, max_weight = 1)),
    format = "rds"
  ),
  tar_target(
    elastic_net_backtest,
    run_backtest("elastic_net", market_data, params = list(top_n = 5, max_weight = 1, train_prop = 0.7)),
    format = "rds"
  ),
  tar_target(
    random_forest_backtest,
    run_backtest("random_forest", market_data, params = list(top_n = 5, max_weight = 1, trees = 400)),
    format = "rds"
  ),
  tar_target(
    xgboost_backtest,
    run_backtest("xgboost", market_data, params = list(top_n = 5, max_weight = 1, trees = 500, learn_rate = 0.05)),
    format = "rds"
  ),
  tar_target(
    experiment_summary,
    tibble::tibble(
      strategy = c("rule_ma_cross", "rule_breakout", "factor_score", "elastic_net", "random_forest", "xgboost"),
      cagr = c(
        rule_ma_cross_backtest$diagnostics$metrics$cagr,
        rule_breakout_backtest$diagnostics$metrics$cagr,
        factor_score_backtest$diagnostics$metrics$cagr,
        elastic_net_backtest$diagnostics$metrics$cagr,
        random_forest_backtest$diagnostics$metrics$cagr,
        xgboost_backtest$diagnostics$metrics$cagr
      ),
      max_drawdown = c(
        rule_ma_cross_backtest$diagnostics$metrics$max_drawdown,
        rule_breakout_backtest$diagnostics$metrics$max_drawdown,
        factor_score_backtest$diagnostics$metrics$max_drawdown,
        elastic_net_backtest$diagnostics$metrics$max_drawdown,
        random_forest_backtest$diagnostics$metrics$max_drawdown,
        xgboost_backtest$diagnostics$metrics$max_drawdown
      ),
      tracking_error = c(
        rule_ma_cross_backtest$diagnostics$metrics$sp500_tracking_error,
        rule_breakout_backtest$diagnostics$metrics$sp500_tracking_error,
        factor_score_backtest$diagnostics$metrics$sp500_tracking_error,
        elastic_net_backtest$diagnostics$metrics$sp500_tracking_error,
        random_forest_backtest$diagnostics$metrics$sp500_tracking_error,
        xgboost_backtest$diagnostics$metrics$sp500_tracking_error
      )
    ),
    format = "rds"
  )
)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(zoo)
  library(scales)
  library(tidyr)
})

load_symbol_universe <- function() {
  tryCatch({
    sym <- quantmod::stockSymbols()
    data.frame(
      Symbol = toupper(sym$Symbol),
      Name   = as.character(sym$Name),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      Symbol = c("AAPL","MSFT","NVDA","AMZN","GOOGL","META","JPM","XOM","PG","JNJ","V","TSLA","BRK-B","COST","PEP","KO","UNH","HD","VZ","CRM","ETN","HON","CAT","DE","WMT","MCD"),
      Name   = c("Apple Inc.","Microsoft Corporation","NVIDIA Corporation","Amazon.com, Inc.","Alphabet Inc.",
                 "Meta Platforms, Inc.","JPMorgan Chase & Co.","Exxon Mobil Corporation","Procter & Gamble Company",
                 "Johnson & Johnson","Visa Inc.","Tesla, Inc.","Berkshire Hathaway Inc.","Costco Wholesale Corporation",
                 "PepsiCo, Inc.","Coca-Cola Company","UnitedHealth Group Incorporated","The Home Depot, Inc.","Verizon Communications Inc.",
                 "Salesforce, Inc.","Eaton Corporation plc","Honeywell International Inc.","Caterpillar Inc.","Deere & Company",
                 "Walmart Inc.","McDonald's Corporation"),
      stringsAsFactors = FALSE
    )
  })
}

SYMS <- load_symbol_universe()

symbol_choices <- setNames(
  SYMS$Symbol,
  paste0(SYMS$Symbol, " — ", ifelse(is.na(SYMS$Name), "", SYMS$Name))
)

resolve_to_symbol <- function(query) {
  if (is.null(query) || is.na(query) || !nzchar(query)) return(NA_character_)
  q <- trimws(toupper(query))
  if (q %in% SYMS$Symbol) return(q)

  d <- adist(q, SYMS$Symbol, ignore.case = TRUE, partial = FALSE)
  best_i <- which.min(d)
  if (length(best_i) == 1 && is.finite(d[best_i]) && d[best_i] <= 2) return(SYMS$Symbol[best_i])

  nm <- SYMS$Name
  pref <- which(startsWith(toupper(nm), q))
  if (length(pref)) return(SYMS$Symbol[pref[1]])
  anym <- grep(q, nm, ignore.case = TRUE)
  if (length(anym)) return(SYMS$Symbol[anym[1]])

  NA_character_
}

hist_VaR <- function(x, conf = 0.95) as.numeric(quantile(x, probs = 1 - conf, type = 7, na.rm = TRUE))
param_VaR <- function(x, conf = 0.95) qnorm(1 - conf, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
mc_VaR    <- function(x, n = 10000, conf = 0.95) {
  mu <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
  as.numeric(quantile(rnorm(n, mean = mu, sd = s), probs = 1 - conf, na.rm = TRUE))
}
boot_VaR  <- function(x, B = 20000, conf = 0.95) as.numeric(quantile(sample(x, size = B, replace = TRUE), probs = 1 - conf, na.rm = TRUE))
hist_ES   <- function(x, conf = 0.95) {
  v <- hist_VaR(x, conf)
  vals <- x[!is.na(x) & x <= v]
  if (!length(vals)) NA_real_ else mean(vals)
}

cagr    <- function(r_xts) exp(sum(r_xts, na.rm = TRUE))^(252 / NROW(r_xts)) - 1
ann_vol <- function(r_xts) sd(r_xts, na.rm = TRUE) * sqrt(252)
sharpe  <- function(r_xts, rf_annual) {
  rf_daily <- log1p(rf_annual) / 252
  (mean(r_xts, na.rm = TRUE) - rf_daily) / sd(r_xts, na.rm = TRUE) * sqrt(252)
}

calc_weights <- function(mode, hist_prices, vol_look = 60, mom_look = 252, weights0 = NULL) {
  N <- NCOL(hist_prices)
  if (N == 0) return(numeric(0))
  if (mode == "equal") {
    rep(1 / N, N)
  } else if (mode == "riskparity") {
    rets <- na.omit(diff(log(tail(hist_prices, vol_look + 1))))
    vols <- apply(rets, 2, sd, na.rm = TRUE)
    vols[!is.finite(vols)] <- median(vols[is.finite(vols)], na.rm = TRUE)
    invv <- 1 / pmax(vols, .Machine$double.eps); invv / sum(invv)
  } else if (mode == "momentum") {
    look <- min(mom_look, max(NROW(hist_prices) - 1, 1))
    pr <- tail(hist_prices, look + 1)
    mom <- as.numeric(pr[NROW(pr), ]) / as.numeric(pr[1, ]) - 1
    mom[!is.finite(mom) | mom < 0] <- 0
    if (sum(mom) == 0) rep(1 / N, N) else mom / sum(mom)
  } else {
    weights0
  }
}

build_port <- function(prices, rebal_freq, weight_mode, vol_look, mom_look, weights0) {
  log_rets <- na.omit(diff(log(prices)))
  if (NCOL(log_rets) == 0) return(list(r_xts = xts(), last_w = weights0))

  if (rebal_freq == "none") {
    w <- calc_weights(weight_mode, prices, vol_look, mom_look, weights0)
    w <- if (is.null(w)) rep(1 / NCOL(prices), NCOL(prices)) else w
    port_ret <- xts(as.matrix(log_rets) %*% matrix(w, ncol = 1), order.by = index(log_rets))
    list(r_xts = port_ret, last_w = w)
  } else {
    eps <- switch(rebal_freq, "months" = endpoints(log_rets, "months"), "quarters" = endpoints(log_rets, "quarters"))
    r_vec <- rep(NA_real_, NROW(log_rets)); last_w <- NULL
    for (i in seq_along(eps[-1])) {
      si <- eps[i] + 1; ei <- eps[i + 1]
      w <- calc_weights(weight_mode, prices[1:ei, ], vol_look, mom_look, weights0)
      w <- if (is.null(w)) rep(1 / NCOL(prices), NCOL(prices)) else w
      r_vec[si:ei] <- as.matrix(log_rets[si:ei, ]) %*% matrix(w, ncol = 1)
      last_w <- w
    }
    port_ret <- xts(r_vec, order.by = index(log_rets)); port_ret <- na.omit(port_ret)
    list(r_xts = port_ret, last_w = last_w)
  }
}

make_plots <- function(r_xts, conf) {
  r <- as.numeric(r_xts)
  VaR_hist  <- hist_VaR(r, conf)
  VaR_param <- param_VaR(r, conf)
  VaR_mc    <- mc_VaR(r, n = 10000, conf = conf)
  VaR_boot  <- boot_VaR(r, B = 20000, conf = conf)
  ES_hist_  <- hist_ES(r, conf)

  cutoffs <- data.frame(Method = c("Hist", "Param", "MC", "Boot", "ES"),
                        xint = c(VaR_hist, VaR_param, VaR_mc, VaR_boot, ES_hist_))
  p_hist <- ggplot(data.frame(r = r), aes(x = r)) +
    geom_histogram(bins = 60) +
    geom_vline(data = cutoffs, aes(xintercept = xint, linetype = Method), linewidth = 0.9) +
    labs(title = paste0(round(conf * 100), "% 1-Day VaR & ES"), x = "Daily log return", y = "Count") +
    theme_minimal()

  roll_hist_var <- tryCatch({
    zoo::rollapply(r_xts, width = 252, by = 1, align = "right",
                   FUN = function(x) hist_VaR(as.numeric(x), conf))
  }, error = function(e) NULL)

  p_roll <- if (!is.null(roll_hist_var) && NROW(roll_hist_var) > 0) {
    ggplot(data.frame(date = index(roll_hist_var), VaR = -as.numeric(roll_hist_var) * 100),
           aes(date, VaR)) + geom_line() +
      labs(title = "Rolling 252-Day Historical VaR", y = "VaR (% loss)", x = NULL) + theme_minimal()
  } else {
    ggplot() + labs(title = "Rolling 252-Day Historical VaR", y = "VaR (% loss)", x = NULL) + theme_minimal()
  }

  equity <- xts(exp(cumsum(r_xts)), order.by = index(r_xts))
  p_eq <- ggplot(data.frame(date = index(equity), eq = as.numeric(equity)), aes(date, eq)) +
    geom_line() + labs(title = "Portfolio Growth", y = "Growth of $1", x = NULL) + theme_minimal()

  dd <- PerformanceAnalytics::Drawdowns(r_xts)
  p_dd <- ggplot(data.frame(date = index(dd), dd = as.numeric(dd) * 100), aes(date, dd)) +
    geom_area() + labs(title = "Drawdown (%)", y = "Drawdown (%)", x = NULL) + theme_minimal()

  list(p_hist = p_hist, p_roll = p_roll, p_eq = p_eq, p_dd = p_dd,
       VaR_hist = VaR_hist, VaR_param = VaR_param, VaR_mc = VaR_mc, VaR_boot = VaR_boot, ES_hist_ = ES_hist_)
}

hhi <- function(w) sum((w) ^ 2)
score_grade <- function(stats, vals, w, conf) {
  sharpe_s <- pmin(pmax((as.numeric(stats["Sharpe", "Value"]) + 0.5) / 1.5, 0), 1)
  var_s    <- pmin(pmax(1 - (abs(vals["Historical"]) / 0.025), 0), 1)
  dd_num   <- as.numeric(stats["MaxDD", "Value"])
  dd_s     <- pmin(pmax(1 - (abs(dd_num) / 0.30), 0), 1)
  hhi_s    <- pmin(pmax(1 - ((hhi(w) - 0.12) / 0.38), 0), 1)
  round((0.4 * sharpe_s + 0.25 * var_s + 0.25 * dd_s + 0.10 * hhi_s) * 100)
}

risk_label <- function(vol, var_pct) {
  if (vol < 0.15 && var_pct < 2.0) "Cautious"
  else if (vol < 0.25 && var_pct < 3.5) "Balanced"
  else "Aggressive"
}

traffic_checks <- function(w, vol, var_pct, maxdd, n) {
  checks <- list()
  checks[["Holdings"]] <- if (n >= 8) "🟢 8+ holdings (diversified)" else if (n >= 5) "🟡 5–7 holdings (okay)" else "🔴 Fewer than 5 holdings (concentrated)"
  max_w <- max(w)
  checks[["Single name"]] <- if (max_w <= 0.25) "🟢 Largest position ≤ 25%" else if (max_w <= 0.4) "🟡 Largest position 26–40%" else "🔴 Largest position > 40%"
  top3 <- sum(sort(w, decreasing = TRUE)[1:min(3, length(w))])
  checks[["Top-3 weight"]] <- if (top3 <= 0.55) "🟢 Top 3 ≤ 55%" else if (top3 <= 0.75) "🟡 Top 3 56–75%" else "🔴 Top 3 > 75%"
  checks[["Volatility"]] <- if (vol < 0.15) "🟢 Low annualized volatility" else if (vol < 0.25) "🟡 Moderate volatility" else "🔴 High volatility"
  checks[["VaR"]] <- if (var_pct < 2.0) "🟢 1-day VaR < 2%" else if (var_pct < 3.5) "🟡 1-day VaR 2–3.5%" else "🔴 1-day VaR > 3.5%"
  checks[["Drawdown"]] <- if (abs(maxdd) < 0.2) "🟢 Max drawdown < 20%" else if (abs(maxdd) < 0.35) "🟡 Max drawdown 20–35%" else "🔴 Max drawdown > 35%"
  unname(unlist(checks))
}

suggestions <- function(w, n, vol, var_pct, maxdd, mode) {
  s <- c()
  if (n < 8) s <- c(s, "Add more tickers (aim for 8–12) to diversify.")
  if (max(w) > 0.25) s <- c(s, "Trim the largest position to ≤ 25%.")
  if (sum(sort(w, TRUE)[1:min(3, n)]) > 0.55) s <- c(s, "Reduce top-3 concentration to ≤ 55%.")
  if (vol > 0.25) s <- c(s, "Consider 'riskparity' weighting to lower volatility.")
  if (var_pct > 3.5) s <- c(s, "Use monthly rebalancing and/or more defensive names to reduce VaR.")
  if (abs(maxdd) > 0.35) s <- c(s, "Blend in lower-beta sectors to improve worst-case behavior.")
  if (length(s) == 0) s <- "Looks solid. Keep weights tidy and rebalance periodically."
  s
}

num_or <- function(x, default = NA_real_) {
  xx <- suppressWarnings(as.numeric(x))
  if (length(xx) == 0 || !is.finite(xx)) default else xx
}

presets <- list(
  "S&P MegaCap Mix" = list(tickers = c("AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "META", "BRK-B", "JPM"), weights = rep(1 / 8, 8)),
  "Dividend Staples" = list(tickers = c("PG", "KO", "PEP", "JNJ", "WMT", "COST", "MCD"), weights = rep(1 / 7, 7)),
  "Equal Tech + Industrials" = list(tickers = c("AAPL", "MSFT", "NVDA", "CRM", "ETN", "HON", "CAT", "DE"), weights = rep(1 / 8, 8))
)

recommended_portfolios <- data.frame(
  Name = names(presets),
  Profile = c("Balanced", "Cautious", "Balanced"),
  Focus = c("Growth & Quality", "Dividend & Defensive", "Growth & Cyclical"),
  Holdings = vapply(presets, function(p) paste(p$tickers, collapse = ", "), character(1)),
  stringsAsFactors = FALSE
)

source("modules/portfolio_module.R")
source("modules/signal_explorer.R")
source("modules/signal_lab_module.R")
source("modules/risk_dashboard_module.R")
source("modules/compliance_module.R")

# Optimizer utilities ------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 0) || all(is.na(x))) y else x
}

default_optimizer_params <- function() {
  list(
    equal     = list(),
    riskparity = list(vol_look = 60),
    momentum  = list(vol_look = 60, mom_look = 252),
    fixed     = list()
  )
}

get_ey_audit_exclusions <- function() {
  ex <- getOption(
    "portfolio.optimizer.ey_exclusions",
    c("COIN", "HOOD", "MSTR", "SI", "MARA", "RIOT", "TSLA")
  )
  unique(toupper(ex))
}

filter_ey_compliant_tickers <- function(tickers, ey_exclusions = get_ey_audit_exclusions()) {
  if (length(tickers) == 0) {
    return(list(allowed = character(0), excluded = character(0)))
  }
  tickers <- unique(toupper(tickers))
  blocked <- intersect(tickers, ey_exclusions)
  allowed <- setdiff(tickers, ey_exclusions)
  list(allowed = allowed, excluded = blocked)
}

calc_optimizer_weights <- function(method, prices, params = list(), weights0 = NULL) {
  method <- tolower(method)
  N <- NCOL(prices)
  if (N == 0) return(numeric(0))

  if (method == "fixed") {
    if (is.null(weights0)) rep(1/N, N) else weights0
  } else if (method == "equal") {
    rep(1/N, N)
  } else if (method == "riskparity") {
    look <- params$vol_look %||% 60
    look <- max(look, 5)
    rets <- na.omit(diff(log(tail(prices, look + 1))))
    vols <- apply(rets, 2, sd, na.rm = TRUE)
    vols[!is.finite(vols)] <- median(vols[is.finite(vols)], na.rm = TRUE)
    invv <- 1 / pmax(vols, .Machine$double.eps)
    invv / sum(invv)
  } else if (method == "momentum") {
    look <- params$mom_look %||% 252
    look <- max(look, 20)
    pr <- tail(prices, min(look + 1, NROW(prices)))
    mom <- as.numeric(pr[NROW(pr), ]) / as.numeric(pr[1, ]) - 1
    mom[!is.finite(mom) | mom < 0] <- 0
    if (sum(mom) == 0) rep(1/N, N) else mom / sum(mom)
  } else {
    stop(sprintf("Unknown optimization method '%s'", method))
  }
}

build_portfolio_returns <- function(prices, rebal_freq = "months", method = "equal",
                                    params = list(), weights0 = NULL) {
  log_rets <- na.omit(diff(log(prices)))
  if (NCOL(log_rets) == 0) {
    return(list(r_xts = xts::xts(), last_w = weights0, rebalance_weights = list()))
  }

  weights_history <- list()
  apply_weights <- function(w, start_idx, end_idx) {
    idx <- seq.int(start_idx, end_idx)
    xts::xts(as.matrix(log_rets[idx, ]) %*% matrix(w, ncol = 1),
             order.by = xts::index(log_rets[idx, ]))
  }

  if (identical(rebal_freq, "none")) {
    w <- calc_optimizer_weights(method, prices, params, weights0)
    if (is.null(w)) w <- rep(1/NCOL(prices), NCOL(prices))
    weights_history[[length(weights_history) + 1]] <- list(date = xts::index(log_rets)[NROW(log_rets)], weights = w)
    port_ret <- apply_weights(w, 1, NROW(log_rets))
    list(r_xts = port_ret, last_w = w, rebalance_weights = weights_history)
  } else {
    eps <- switch(
      rebal_freq,
      months = xts::endpoints(log_rets, "months"),
      quarters = xts::endpoints(log_rets, "quarters"),
      stop(sprintf("Unsupported rebalance frequency '%s'", rebal_freq))
    )
    eps <- unique(c(0, eps))
    r_vec <- rep(NA_real_, NROW(log_rets))
    last_w <- weights0
    for (i in seq_len(length(eps) - 1)) {
      si <- eps[i] + 1
      ei <- eps[i + 1]
      if (ei <= si) next
      w <- calc_optimizer_weights(method, prices[1:(ei + 1), ], params, weights0)
      if (is.null(w)) w <- rep(1/NCOL(prices), NCOL(prices))
      r_vec[si:ei] <- as.matrix(log_rets[si:ei, ]) %*% matrix(w, ncol = 1)
      last_w <- w
      weights_history[[length(weights_history) + 1]] <- list(date = xts::index(log_rets)[ei], weights = w)
    }
    port_ret <- xts::xts(r_vec, order.by = xts::index(log_rets))
    port_ret <- stats::na.omit(port_ret)
    list(r_xts = port_ret, last_w = last_w, rebalance_weights = weights_history)
  }
}

optimize_portfolio <- function(prices, method, rebal_freq = "months", params = list(),
                               weights0 = NULL, risk_free = 0.02, risk_window = 252) {
  built <- build_portfolio_returns(prices, rebal_freq, method, params, weights0)
  r_xts <- built$r_xts
  last_w <- built$last_w
  if (length(last_w) == 0) last_w <- rep(1/NCOL(prices), NCOL(prices))

  trailing <- tail(na.omit(diff(log(prices))), risk_window)
  mu <- colMeans(trailing, na.rm = TRUE)
  cov_mat <- stats::cov(trailing, use = "pairwise.complete.obs")
  mu[!is.finite(mu)] <- 0
  if (!all(is.finite(cov_mat))) cov_mat <- stats::cov(na.omit(diff(log(prices))), use = "pairwise.complete.obs")

  exp_daily <- sum(last_w * mu)
  exp_ann <- exp_daily * 252
  exp_vol <- sqrt(drop(t(last_w) %*% cov_mat %*% last_w) * 252)
  exp_sharpe <- if (isTRUE(exp_vol > 0)) (exp_ann - risk_free) / exp_vol else NA_real_

  assets <- colnames(prices)
  asset_mu <- mu * 252
  weight_summary <- data.frame(
    Ticker = assets,
    Weight = round(last_w, 4),
    ExpReturn = round(asset_mu, 4),
    stringsAsFactors = FALSE
  )

  list(
    r_xts = r_xts,
    last_w = last_w,
    rebalance_weights = built$rebalance_weights,
    expected_annual_return = exp_ann,
    expected_annual_vol = exp_vol,
    expected_sharpe = exp_sharpe,
    asset_expected = weight_summary
  )
}

simulate_event_impact <- function(prices, weights, scenarios = c("fed_cut", "earnings_shock", "peer_move"),
                                  window = 126, focus_ticker = NULL, peer_ticker = NULL,
                                  fed_cut_move = 0.012, earnings_shock = -0.08, peer_move = 0.05) {
  if (length(weights) == 0 || NCOL(prices) == 0) {
    return(data.frame())
  }
  scenarios <- unique(match.arg(scenarios, several.ok = TRUE))
  returns <- na.omit(diff(log(prices)))
  if (NROW(returns) == 0) return(data.frame())
  recent <- tail(returns, window)
  assets <- colnames(recent)
  weights <- weights[assets]
  weights[is.na(weights)] <- 0
  if (!isTRUE(sum(weights) > 0)) {
    weights[] <- 1 / length(weights)
  } else {
    weights <- weights / sum(weights)
  }

  mu <- colMeans(recent, na.rm = TRUE)
  cov_mat <- stats::cov(recent, use = "pairwise.complete.obs")
  base_daily <- sum(weights * mu)
  base_ann <- base_daily * 252
  base_vol <- sqrt(drop(t(weights) %*% cov_mat %*% weights) * 252)

  results <- lapply(scenarios, function(scn) {
    shock_vec <- rep(0, length(assets))
    names(shock_vec) <- assets
    notes <- ""

    if (scn == "fed_cut") {
      tlt <- tryCatch({
        suppressWarnings(quantmod::getSymbols(
          "TLT",
          from = as.character(start(prices)),
          to = as.character(end(prices)),
          auto.assign = FALSE
        ))
      }, error = function(e) NULL)
      if (!is.null(tlt)) {
        tlt_ret <- na.omit(diff(log(quantmod::Ad(tlt))))
        tlt_ret <- tlt_ret[index(recent)]
        corr <- vapply(assets, function(sym) stats::cor(recent[, sym], tlt_ret, use = "complete.obs"), numeric(1))
        corr[!is.finite(corr)] <- 0
        shock_vec <- fed_cut_move * corr
        notes <- "Scaled by correlation to TLT (rate sensitivity)."
      } else {
        shock_vec[] <- fed_cut_move * 0.6
        notes <- "TLT unavailable; applied uniform pro-growth lift."
      }
    } else if (scn == "earnings_shock") {
      focus <- focus_ticker %||% assets[1]
      focus <- intersect(focus, assets)[1]
      if (!length(focus)) {
        shock_vec[] <- 0
        notes <- "Focus ticker missing from price window."
      } else {
        shock_vec[focus] <- earnings_shock
        peer_corr <- vapply(assets, function(sym) stats::cor(recent[, sym], recent[, focus], use = "complete.obs"), numeric(1))
        peer_corr[!is.finite(peer_corr)] <- 0
        shock_vec <- shock_vec + earnings_shock * 0.5 * peer_corr
        notes <- sprintf("%s shock propagated via trailing correlations.", focus)
      }
    } else if (scn == "peer_move") {
      peer <- peer_ticker %||% assets[1]
      peer <- intersect(peer, assets)[1]
      if (!length(peer)) {
        shock_vec[] <- 0
        notes <- "Peer ticker missing from price window."
      } else {
        peer_corr <- vapply(assets, function(sym) stats::cor(recent[, sym], recent[, peer], use = "complete.obs"), numeric(1))
        peer_corr[!is.finite(peer_corr)] <- 0
        shock_vec <- peer_move * peer_corr
        shock_vec[peer] <- peer_move
        notes <- sprintf("Peer %s move cascaded via trailing correlations.", peer)
      }
    }

    port_impact <- sum(weights * shock_vec)
    data.frame(
      Scenario = scn,
      PortfolioImpactPct = round(port_impact * 100, 2),
      BaseAnnReturnPct = round(base_ann * 100, 2),
      BaseAnnVolPct = round(base_vol * 100, 2),
      Notes = notes,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

# Helper utilities for feature engineering and signal discovery
# -----------------------------------------------------------------------------
# This module adds data wrangling helpers that can be sourced from the Shiny
# server or other scripts without pulling in heavyweight dependencies. All
# functions are written using base R / zoo primitives so that they work with
# the existing xts objects used across the application.

#' Compute lagged returns for each column in a price series
#'
#' @param prices xts/zoo/data.frame with price levels
#' @param lags integer vector of lookback periods (in rows) to compute
#' @param method "log" for log returns or "simple" for arithmetic returns
#' @return xts/data.frame with one column per asset/lag combination
lagged_returns <- function(prices, lags = c(1L, 5L, 20L), method = c("log", "simple")) {
  method <- match.arg(method)
  if (is.null(prices)) stop("`prices` must be provided")

  mat <- as.matrix(prices)
  if (!is.numeric(mat)) stop("`prices` must be coercible to numeric matrix")
  if (!nrow(mat) || !ncol(mat)) {
    out <- matrix(numeric(0), nrow = nrow(mat), ncol = 0)
    rownames(out) <- rownames(mat)
    if (inherits(prices, "xts")) {
      return(xts::xts(out, order.by = xts::index(prices)))
    }
    return(out)
  }
  coln <- colnames(mat)
  if (is.null(coln)) coln <- paste0("series", seq_len(ncol(mat)))

  lags <- unique(as.integer(lags))
  lags <- lags[lags >= 1]
  if (!length(lags)) stop("`lags` must contain positive integers")

  shift_matrix <- function(m, lag) {
    n <- nrow(m)
    if (!n) return(matrix(NA_real_, nrow = 0, ncol = ncol(m)))
    if (lag >= n) return(matrix(NA_real_, nrow = n, ncol = ncol(m)))
    rbind(matrix(NA_real_, nrow = lag, ncol = ncol(m)), m[seq_len(n - lag), , drop = FALSE])
  }

  n <- nrow(mat)
  out <- matrix(NA_real_, nrow = n, ncol = length(lags) * ncol(mat))
  out_colnames <- character(length(lags) * ncol(mat))

  idx <- 1L
  for (lag in lags) {
    shifted <- shift_matrix(mat, lag)
    if (method == "log") {
      rets <- log(mat) - log(shifted)
    } else {
      rets <- mat / shifted - 1
    }
    rets[!is.finite(rets)] <- NA_real_
    cols <- seq_len(ncol(mat)) + idx - 1L
    out[, cols] <- rets
    out_colnames[cols] <- paste0(coln, "_lag", lag)
    idx <- idx + ncol(mat)
  }

  colnames(out) <- out_colnames
  rownames(out) <- rownames(mat)
  if (inherits(prices, "xts")) {
    out <- xts::xts(out, order.by = xts::index(prices))
  }
  out
}

#' Rolling correlation between two series
#'
#' @param x,y Numeric vectors/xts of equal length
#' @param window Rolling window size in observations
#' @param min_obs Minimum overlapping observations required for a value
#' @param method Correlation method passed to stats::cor
#' @return Numeric vector/xts with rolling correlations
rolling_corr <- function(x, y, window = 60L, min_obs = ceiling(window / 2), method = c("pearson", "spearman")) {
  if (length(x) != length(y)) stop("`x` and `y` must be the same length")
  method <- match.arg(method)
  window <- as.integer(window)
  if (window <= 1) stop("`window` must be > 1")

  x_xts <- inherits(x, "xts")
  idx <- if (x_xts) xts::index(x) else NULL
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (!n) {
    if (x_xts) return(x[0])
    return(numeric(0))
  }
  res <- rep(NA_real_, n)
  window <- min(window, n)
  min_obs <- min(as.integer(min_obs), window)

  for (end in seq_len(n)) {
    start <- max(1, end - window + 1)
    xs <- x[start:end]
    ys <- y[start:end]
    valid <- stats::complete.cases(xs, ys)
    if (sum(valid) < min_obs) next
    res[end] <- stats::cor(xs[valid], ys[valid], method = method)
  }

  if (x_xts) {
    res <- xts::xts(res, order.by = idx)
  }
  res
}

shift_series <- function(vec, k) {
  vec <- as.numeric(vec)
  n <- length(vec)
  if (!n) return(vec)
  k <- as.integer(k)
  if (k > 0) {
    if (k >= n) return(rep(NA_real_, n))
    c(rep(NA_real_, k), vec[seq_len(n - k)])
  } else if (k < 0) {
    k <- abs(k)
    if (k >= n) return(rep(NA_real_, n))
    c(vec[(k + 1):n], rep(NA_real_, k))
  } else {
    vec
  }
}

#' Rolling beta of a target series against factors
#'
#' @param target Numeric vector/xts of asset returns
#' @param factor Numeric vector/xts of factor returns
#' @param window Rolling window size
#' @return Numeric vector/xts of rolling betas
rolling_beta <- function(target, factor, window = 60L, min_obs = ceiling(window / 2)) {
  if (length(target) != length(factor)) stop("`target` and `factor` must be the same length")
  window <- as.integer(window)
  if (window <= 1) stop("`window` must be > 1")

  target_xts <- inherits(target, "xts")
  idx <- if (target_xts) xts::index(target) else NULL
  target <- as.numeric(target)
  factor <- as.numeric(factor)
  n <- length(target)
  if (!n) {
    if (target_xts) return(target[0])
    return(numeric(0))
  }
  res <- rep(NA_real_, n)
  window <- min(window, n)
  min_obs <- min(as.integer(min_obs), window)

  for (end in seq_len(n)) {
    start <- max(1, end - window + 1)
    ys <- target[start:end]
    xs <- factor[start:end]
    valid <- stats::complete.cases(xs, ys)
    if (sum(valid) < min_obs) next
    xs <- xs[valid]
    ys <- ys[valid]
    xs <- xs - mean(xs)
    ys <- ys - mean(ys)
    denom <- sum(xs^2)
    if (!denom) next
    res[end] <- sum(xs * ys) / denom
  }

  if (target_xts) {
    res <- xts::xts(res, order.by = idx)
  }
  res
}

#' Compute macro surprise scores from actual vs forecast releases
#'
#' @param releases data.frame with actual/forecast columns
#' @param actual_col Column name for actual prints
#' @param forecast_col Column name for expectations
#' @param dispersion_col Optional column for survey dispersion (used as denom)
#' @param window Rolling window (if dispersion is missing) to standardise surprises
#' @return data.frame with added `surprise` column
macro_surprise_score <- function(releases,
                                 actual_col = "actual",
                                 forecast_col = "forecast",
                                 dispersion_col = NULL,
                                 window = 12L) {
  if (!is.data.frame(releases)) stop("`releases` must be a data.frame")
  if (!all(c(actual_col, forecast_col) %in% names(releases))) {
    stop("Columns not found in releases data")
  }

  actual <- as.numeric(releases[[actual_col]])
  forecast <- as.numeric(releases[[forecast_col]])
  spread <- actual - forecast

  if (!is.null(dispersion_col) && dispersion_col %in% names(releases)) {
    denom <- as.numeric(releases[[dispersion_col]])
  } else {
    window <- max(as.integer(window), 3L)
    denom <- zoo::rollapply(spread, width = window, align = "right", fill = NA,
                            FUN = function(x) stats::sd(stats::na.omit(x)))
  }
  denom[denom == 0] <- NA_real_
  surprise <- spread / denom
  releases$surprise <- surprise
  releases
}

#' Compute naive sentiment scores from free-form text
#'
#' @param text Vector of character strings
#' @param lexicon Named numeric vector mapping tokens to scores
#' @param normalize Whether to normalise by token count (TRUE)
#' @return numeric vector of sentiment scores
sentiment_score <- function(text,
                            lexicon = c(
                              bullish = 1, buy = 0.5, growth = 0.4, gain = 0.35,
                              bearish = -1, sell = -0.5, risk = -0.4, loss = -0.35,
                              positive = 0.3, negative = -0.3
                            ),
                            normalize = TRUE) {
  if (!length(text)) return(numeric(0))
  if (is.null(names(lexicon))) stop("`lexicon` must be a named numeric vector")
  lexicon <- lexicon[is.finite(lexicon)]

  clean_text <- tolower(gsub("[^a-z0-9\n\r\t ]", " ", text))
  scores <- vapply(clean_text, function(doc) {
    tokens <- unlist(strsplit(doc, "\\s+", perl = TRUE))
    tokens <- tokens[nzchar(tokens)]
    if (!length(tokens)) return(0)
    vals <- lexicon[tokens]
    vals <- vals[!is.na(vals)]
    if (!length(vals)) return(0)
    score <- sum(vals)
    if (normalize) score <- score / length(tokens)
    score
  }, numeric(1))
  scores
}

# -----------------------------------------------------------------------------
# Discovery routines

ensure_time_series_matrix <- function(x) {
  if (inherits(x, "xts")) {
    return(list(data = as.matrix(x), index = xts::index(x), colnames = colnames(x)))
  }
  mat <- as.matrix(x)
  idx <- if (!is.null(rownames(mat))) rownames(mat) else NULL
  list(data = mat, index = idx, colnames = colnames(mat))
}

#' Find lead/lag relationships between series
#'
#' @param series Matrix/data.frame/xts with columns as series
#' @param max_lag Maximum lag (in rows) to test in either direction
#' @param min_corr Minimum absolute correlation to include
#' @param method Correlation type (pearson/spearman)
#' @param top_n Number of candidates to return
#' @param persist_path Optional path to persist results (qs or rds)
#' @return data.frame of ranked candidates
find_lead_lag_pairs <- function(series,
                                max_lag = 5L,
                                min_corr = 0.2,
                                method = c("pearson", "spearman"),
                                top_n = 25L,
                                persist_path = NULL) {
  method <- match.arg(method)
  info <- ensure_time_series_matrix(series)
  data <- info$data
  cn <- info$colnames
  if (is.null(cn)) cn <- paste0("series", seq_len(ncol(data)))

  max_lag <- as.integer(max_lag)
  if (max_lag < 1) stop("`max_lag` must be >= 1")

  n <- ncol(data)
  results <- list()
  ridx <- 1L

  for (i in seq_len(n)) {
    x <- data[, i]
    for (j in seq_len(n)) {
      if (i == j || j <= i) next
      y <- data[, j]
      for (lag in seq_len(max_lag)) {
        y_shift <- shift_series(y, lag)
        complete <- stats::complete.cases(x, y_shift)
        if (sum(complete) >= 15) {
          corr <- tryCatch(stats::cor(x[complete], y_shift[complete], method = method),
                           error = function(e) NA_real_)
          if (is.finite(corr) && abs(corr) >= min_corr) {
            test <- tryCatch(stats::cor.test(x[complete], y_shift[complete], method = method),
                             error = function(e) NULL)
            pval <- if (is.null(test)) NA_real_ else test$p.value
            results[[ridx]] <- data.frame(
              signal_type = "lead_lag",
              leader = cn[i],
              follower = cn[j],
              lag = lag,
              correlation = corr,
              p_value = pval,
              observations = sum(complete),
              stringsAsFactors = FALSE
            )
            ridx <- ridx + 1L
          }
        }

        x_shift <- shift_series(x, lag)
        complete_xy <- stats::complete.cases(x_shift, y)
        if (sum(complete_xy) >= 15) {
          corr_xy <- tryCatch(stats::cor(x_shift[complete_xy], y[complete_xy], method = method),
                              error = function(e) NA_real_)
          if (is.finite(corr_xy) && abs(corr_xy) >= min_corr) {
            test_xy <- tryCatch(stats::cor.test(x_shift[complete_xy], y[complete_xy], method = method),
                                error = function(e) NULL)
            pval_xy <- if (is.null(test_xy)) NA_real_ else test_xy$p.value
            results[[ridx]] <- data.frame(
              signal_type = "lead_lag",
              leader = cn[j],
              follower = cn[i],
              lag = lag,
              correlation = corr_xy,
              p_value = pval_xy,
              observations = sum(complete_xy),
              stringsAsFactors = FALSE
            )
            ridx <- ridx + 1L
          }
        }
      }
    }
  }

  if (!length(results)) return(data.frame())
  out <- do.call(rbind, results)
  out <- out[order(-abs(out$correlation), out$p_value), , drop = FALSE]
  if (!is.null(top_n)) {
    top_n <- as.integer(top_n)
    if (top_n > 0 && nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  }

  out$generated_at <- Sys.time()
  if (!is.null(persist_path)) {
    persist_discovered_signals(out, path = persist_path)
  }
  rownames(out) <- NULL
  out
}

#' Scan factor sensitivities for a panel of assets
#'
#' @param asset_returns Matrix/data.frame/xts of asset returns
#' @param factor_returns Matrix/data.frame/xts of factor returns
#' @param lookback Optional integer of rows to use from the tail
#' @param min_obs Minimum observations required for regression
#' @param top_n Number of strongest exposures to return
#' @param persist_path Optional path to persist results
#' @return data.frame with beta/t-statistics per asset-factor pair
scan_factor_sensitivity <- function(asset_returns,
                                    factor_returns,
                                    lookback = NULL,
                                    min_obs = 60L,
                                    top_n = 25L,
                                    persist_path = NULL) {
  asset_info <- ensure_time_series_matrix(asset_returns)
  factor_info <- ensure_time_series_matrix(factor_returns)

  asset_mat <- asset_info$data
  factor_mat <- factor_info$data

  if (!is.null(asset_info$index) && !is.null(factor_info$index)) {
    common_idx <- intersect(asset_info$index, factor_info$index)
    if (!length(common_idx)) return(data.frame())
    common_idx <- sort(unique(common_idx))
    asset_keep <- match(common_idx, asset_info$index)
    factor_keep <- match(common_idx, factor_info$index)
    asset_keep <- asset_keep[!is.na(asset_keep)]
    factor_keep <- factor_keep[!is.na(factor_keep)]
    if (!length(asset_keep) || !length(factor_keep)) return(data.frame())
    len <- min(length(asset_keep), length(factor_keep))
    asset_mat <- asset_mat[asset_keep[seq_len(len)], , drop = FALSE]
    factor_mat <- factor_mat[factor_keep[seq_len(len)], , drop = FALSE]
  } else {
    common_rows <- min(nrow(asset_mat), nrow(factor_mat))
    asset_mat <- asset_mat[seq_len(common_rows), , drop = FALSE]
    factor_mat <- factor_mat[seq_len(common_rows), , drop = FALSE]
  }

  if (!is.null(lookback)) {
    lookback <- min(as.integer(lookback), nrow(asset_mat))
    asset_mat <- tail(asset_mat, lookback)
    factor_mat <- tail(factor_mat, lookback)
  }

  asset_names <- asset_info$colnames
  if (is.null(asset_names)) asset_names <- paste0("asset", seq_len(ncol(asset_mat)))
  factor_names <- factor_info$colnames
  if (is.null(factor_names)) factor_names <- paste0("factor", seq_len(ncol(factor_mat)))

  results <- list()
  ridx <- 1L

  for (i in seq_len(ncol(asset_mat))) {
    y <- asset_mat[, i]
    for (j in seq_len(ncol(factor_mat))) {
      x <- factor_mat[, j]
      complete <- stats::complete.cases(y, x)
      if (sum(complete) < min_obs) next
      yy <- y[complete]
      xx <- x[complete]
      X <- cbind(1, xx)
      fit <- stats::lm.fit(x = X, y = yy)
      if (fit$rank < 2) next
      df_resid <- length(yy) - fit$rank
      if (df_resid <= 0) next
      rss <- sum(fit$residuals^2)
      s2 <- rss / df_resid
      XtX_inv <- tryCatch(stats::chol2inv(qr.R(fit$qr)), error = function(e) NULL)
      if (is.null(XtX_inv)) next
      se <- sqrt(diag(XtX_inv) * s2)
      beta <- fit$coefficients[2]
      beta_se <- se[2]
      if (!is.finite(beta) || !is.finite(beta_se) || beta_se == 0) next
      t_stat <- beta / beta_se
      pval <- 2 * stats::pt(-abs(t_stat), df_resid)
      rsq <- 1 - rss / sum((yy - mean(yy))^2)
      results[[ridx]] <- data.frame(
        signal_type = "factor_sensitivity",
        asset = asset_names[i],
        factor = factor_names[j],
        beta = beta,
        beta_se = beta_se,
        t_stat = t_stat,
        p_value = pval,
        r_squared = rsq,
        observations = sum(complete),
        stringsAsFactors = FALSE
      )
      ridx <- ridx + 1L
    }
  }

  if (!length(results)) return(data.frame())
  out <- do.call(rbind, results)
  out <- out[order(-abs(out$t_stat), out$p_value), , drop = FALSE]
  if (!is.null(top_n)) {
    top_n <- as.integer(top_n)
    if (top_n > 0 && nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  }
  out$generated_at <- Sys.time()
  if (!is.null(persist_path)) {
    persist_discovered_signals(out, path = persist_path)
  }
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# Persistence helpers

persist_discovered_signals <- function(signals,
                                       path = file.path("data", "derived_signals.qs"),
                                       append = TRUE,
                                       fallback_rds = TRUE) {
  if (!is.data.frame(signals) || !nrow(signals)) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (!"signal_type" %in% names(signals)) {
    signals$signal_type <- "unknown"
  }
  if (!"generated_at" %in% names(signals)) {
    signals$generated_at <- Sys.time()
  }
  signals$generated_at <- as.POSIXct(signals$generated_at, tz = "UTC")
  signals$generated_at[is.na(signals$generated_at)] <- Sys.time()
  if (!"persisted_at" %in% names(signals)) {
    signals$persisted_at <- Sys.time()
  }
  signals$persisted_at <- as.POSIXct(signals$persisted_at, tz = "UTC")
  signals$persisted_at[is.na(signals$persisted_at)] <- Sys.time()

  existing <- NULL
  if (append) {
    if (file.exists(path) && requireNamespace("qs", quietly = TRUE)) {
      existing <- tryCatch(qs::qread(path), error = function(e) NULL)
    } else if (fallback_rds) {
      alt <- sub("\\.qs$", ".rds", path)
      if (file.exists(alt)) {
        existing <- tryCatch(readRDS(alt), error = function(e) NULL)
      }
    }
  }

  if (is.data.frame(existing) && nrow(existing)) {
    existing_cols <- names(existing)
    signal_cols <- names(signals)
    all_cols <- union(existing_cols, signal_cols)
    for (nm in setdiff(all_cols, existing_cols)) existing[[nm]] <- NA
    for (nm in setdiff(all_cols, signal_cols)) signals[[nm]] <- NA
    combined <- rbind(existing[, all_cols], signals[, all_cols])
  } else {
    combined <- signals
  }

  if (requireNamespace("qs", quietly = TRUE)) {
    qs::qsave(combined, path)
    invisible(path)
  } else if (fallback_rds) {
    alt <- sub("\\.qs$", ".rds", path)
    saveRDS(combined, alt)
    warning(sprintf("`qs` package not available, saved signals to %s instead", alt))
    invisible(alt)
  } else {
    stop("`qs` package not installed and fallback disabled")
  }
}

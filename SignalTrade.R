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
})

source("R/portfolio/optimizer.R")

# --- Symbol universe for search & correction ---
load_symbol_universe <- function() {
  tryCatch({
    sym <- quantmod::stockSymbols()
    df <- data.frame(
      Symbol = toupper(sym$Symbol),
      Name   = as.character(sym$Name),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    df <- data.frame(
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
  df <- subset(df, !Symbol %in% get_ey_audit_exclusions())
  rownames(df) <- NULL
  df
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

# ---------- Core risk helpers ----------
hist_VaR <- function(x, conf=0.95) as.numeric(quantile(x, probs=1-conf, type=7, na.rm=TRUE))
param_VaR <- function(x, conf=0.95) qnorm(1-conf, mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE))
mc_VaR    <- function(x, n=10000, conf=0.95) {
  mu <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE)
  as.numeric(quantile(rnorm(n, mean=mu, sd=s), probs=1-conf, na.rm=TRUE))
}
boot_VaR  <- function(x, B=20000, conf=0.95) as.numeric(quantile(sample(x, size=B, replace=TRUE), probs=1-conf, na.rm=TRUE))
hist_ES   <- function(x, conf=0.95) {
  v <- hist_VaR(x, conf)
  vals <- x[!is.na(x) & x <= v]
  if (!length(vals)) NA_real_ else mean(vals)
}

cagr    <- function(r_xts) exp(sum(r_xts, na.rm=TRUE))^(252/NROW(r_xts)) - 1
ann_vol <- function(r_xts) sd(r_xts, na.rm=TRUE) * sqrt(252)
sharpe  <- function(r_xts, rf_annual) {
  rf_daily <- log1p(rf_annual)/252
  (mean(r_xts, na.rm=TRUE) - rf_daily) / sd(r_xts, na.rm=TRUE) * sqrt(252)
}

make_plots <- function(r_xts, conf) {
  r <- as.numeric(r_xts)
  VaR_hist  <- hist_VaR(r, conf)
  VaR_param <- param_VaR(r, conf)
  VaR_mc    <- mc_VaR(r, n=10000, conf=conf)
  VaR_boot  <- boot_VaR(r, B=20000, conf=conf)
  ES_hist_  <- hist_ES(r, conf)
  
  cutoffs <- data.frame(Method=c("Hist","Param","MC","Boot","ES"),
                        xint=c(VaR_hist, VaR_param, VaR_mc, VaR_boot, ES_hist_))
  p_hist <- ggplot(data.frame(r=r), aes(x=r)) +
    geom_histogram(bins=60) +
    geom_vline(data=cutoffs, aes(xintercept=xint, linetype=Method), linewidth=0.9) +
    labs(title=paste0(round(conf*100), "% 1-Day VaR & ES"), x="Daily log return", y="Count") +
    theme_minimal()
  
  # Rolling Historical VaR on returns
  roll_hist_var <- tryCatch({
    zoo::rollapply(r_xts, width=252, by=1, align="right",
                   FUN=function(x) hist_VaR(as.numeric(x), conf))
  }, error=function(e) NULL)
  
  p_roll <- if (!is.null(roll_hist_var) && NROW(roll_hist_var)>0) {
    ggplot(data.frame(date=index(roll_hist_var), VaR=-as.numeric(roll_hist_var)*100),
           aes(date, VaR)) + geom_line() +
      labs(title="Rolling 252-Day Historical VaR", y="VaR (% loss)", x=NULL) + theme_minimal()
  } else {
    ggplot() + labs(title="Rolling 252-Day Historical VaR", y="VaR (% loss)", x=NULL) + theme_minimal()
  }
  
  equity <- xts(exp(cumsum(r_xts)), order.by=index(r_xts))
  p_eq <- ggplot(data.frame(date=index(equity), eq=as.numeric(equity)), aes(date, eq)) +
    geom_line() + labs(title="Portfolio Growth", y="Growth of $1", x=NULL) + theme_minimal()
  
  # Drawdowns correctly from returns (not equity)
  dd <- PerformanceAnalytics::Drawdowns(r_xts)
  p_dd <- ggplot(data.frame(date=index(dd), dd=as.numeric(dd)*100), aes(date, dd)) +
    geom_area() + labs(title="Drawdown (%)", y="Drawdown (%)", x=NULL) + theme_minimal()
  
  list(p_hist=p_hist, p_roll=p_roll, p_eq=p_eq, p_dd=p_dd,
       VaR_hist=VaR_hist, VaR_param=VaR_param, VaR_mc=VaR_mc, VaR_boot=VaR_boot, ES_hist_=ES_hist_)
}

# ---------- Scoring & guidance ----------
hhi <- function(w) sum((w)^2)
score_grade <- function(stats, vals, w, conf) {
  sharpe_s <- pmin(pmax((as.numeric(stats["Sharpe","Value"])+0.5)/1.5, 0), 1)
  var_s    <- pmin(pmax(1 - (abs(vals["Historical"]) / 0.025), 0), 1)
  dd_num   <- as.numeric(stats["MaxDD","Value"])
  dd_s     <- pmin(pmax(1 - (abs(dd_num) / 0.30), 0), 1)
  hhi_s    <- pmin(pmax(1 - ((hhi(w) - 0.12) / 0.38), 0), 1)
  round((0.4*sharpe_s + 0.25*var_s + 0.25*dd_s + 0.10*hhi_s) * 100)
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
  top3 <- sum(sort(w, decreasing=TRUE)[1:min(3,length(w))])
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
  if (sum(sort(w, TRUE)[1:min(3,n)]) > 0.55) s <- c(s, "Reduce top-3 concentration to ≤ 55%.")
  if (vol > 0.25) s <- c(s, "Consider 'riskparity' weighting to lower volatility.")
  if (var_pct > 3.5) s <- c(s, "Use monthly rebalancing and/or more defensive names to reduce VaR.")
  if (abs(maxdd) > 0.35) s <- c(s, "Blend in lower-beta sectors to improve worst-case behavior.")
  if (length(s) == 0) s <- "Looks solid. Keep weights tidy and rebalance periodically."
  s
}

# ---------- Presets ----------
presets <- list(
  "S&P MegaCap Mix" = list(tickers=c("AAPL","MSFT","NVDA","AMZN","GOOGL","META","BRK-B","JPM"), weights=rep(1/8,8)),
  "Dividend Staples" = list(tickers=c("PG","KO","PEP","JNJ","WMT","COST","MCD"), weights=rep(1/7,7)),
  "Equal Tech + Industrials" = list(tickers=c("AAPL","MSFT","NVDA","CRM","ETN","HON","CAT","DE"), weights=rep(1/8,8))
)

# ---------- UI ----------
theme <- bs_theme(bootswatch="minty", base_font = bslib::font_google("Inter"))
ui <- page_navbar(
  title = "Portfolio Risk Lab",
  theme = theme,
  fillable = TRUE,
  nav_panel("Analyze",
            layout_sidebar(
              sidebar = sidebar(
                width = 380,
                h4("Quick start"),
                radioButtons("preset", NULL, choices = c("None", names(presets)), inline = FALSE, selected = "None"),
                h4("Build your portfolio"),
                selectizeInput(
                  "tickers", "Tickers",
                  choices = symbol_choices,
                  multiple = TRUE,
                  options = list(
                    create = TRUE,
                    persist = TRUE,
                    selectOnTab = TRUE,
                    createOnBlur = TRUE,
                    placeholder = "Type company or symbol… (e.g., apple → AAPL)",
                    plugins = list("remove_button"),
                    render = I("{
                      option: function(item, escape) { return '<div>' + escape(item.label) + '</div>'; },
                      item:   function(item, escape) { return '<div>' + escape(item.label) + '</div>'; }
                    }")
                  )
                ),
                radioButtons("risk_profile", "Risk profile", choices=c("Cautious","Balanced","Aggressive"), inline=TRUE, selected="Balanced"),
                dateRangeInput("dates", "Date range", start = "2020-01-01", end = Sys.Date()),
                sliderInput("conf", "VaR/ES confidence", min=0.80, max=0.995, value=0.95, step=0.005),
                numericInput("notional", "Notional ($)", 100000, min=1000, step=1000),
                selectInput("rebal", "Rebalance", c("none","months","quarters"), selected="months"),
                selectInput("mode", "Weighting", c("fixed","equal","riskparity","momentum"), selected="equal"),
                conditionalPanel(
                  condition = "input.mode == 'fixed'",
                  textAreaInput("weights", "Fixed weights (comma-separated; must sum to 1)", rows = 3,
                                placeholder = "e.g. 0.2, 0.2, 0.2, 0.2, 0.2"),
                  actionButton("equalize", "Equalize weights for selected tickers")
                ),
                hr(),
                actionButton("run", "Run analysis", class = "btn-primary"),
                br(), br(),
                downloadButton("download_pdf", "Download PDF")
              ),
              # Score & Guidance
              fluidRow(
                column(4,
                       card(
                         card_header("Portfolio Grade"),
                         h1(textOutput("grade_score"), style="margin-top:0"),
                         tags$div(textOutput("grade_label"), style="font-weight:600;margin-bottom:8px;"),
                         tags$small("Grade blends Sharpe, VaR, max drawdown, and concentration (HHI).")
                       )
                ),
                column(8,
                       card(
                         card_header("Traffic-light checks"),
                         uiOutput("checks_ui"),
                         card(
                           card_header("Suggestions"),
                           uiOutput("suggestions_ui")
                         )
                       )
                )
              ),
              card(
                card_header("Risk Summary"),
                fluidRow(
                  column(6, DTOutput("risk_tbl")),
                  column(6, DTOutput("stats_tbl"))
                )
              ),
              card(
                card_header("Optimization Snapshot"),
                fluidRow(
                  column(4, uiOutput("opt_metrics")),
                  column(4, DTOutput("weights_tbl")),
                  column(4, uiOutput("compliance_ui"))
                )
              ),
              card(
                card_header("Stress Tests"),
                DTOutput("stress_tbl")
              ),
              card(
                card_header("Distributions & Risk Over Time"),
                fluidRow(
                  column(6, plotOutput("p_hist", height = 300)),
                  column(6, plotOutput("p_roll", height = 300))
                )
              ),
              card(
                card_header("Growth & Drawdowns"),
                fluidRow(
                  column(6, plotOutput("p_eq", height = 300)),
                  column(6, plotOutput("p_dd", height = 300))
                )
              )
            )
  ),
  nav_spacer(),
  nav_item(a(href="https://finance.yahoo.com", target="_blank", "Data via Yahoo Finance"))
)

# ---------- Server ----------
server <- function(input, output, session) {
  num_or <- function(x, default = NA_real_) {
    xx <- suppressWarnings(as.numeric(x))
    if (length(xx) == 0 || !is.finite(xx)) default else xx
  }
  
  updateSelectizeInput(session, "tickers", choices = symbol_choices, server = TRUE, selected = NULL)
  
  observeEvent(input$tickers, {
    req(input$tickers)
    corrected <- vapply(input$tickers, resolve_to_symbol, character(1))
    bad <- which(is.na(corrected))
    if (length(bad)) {
      fixed <- corrected[!is.na(corrected)]
      if (length(fixed) == 0) {
        updateSelectizeInput(session, "tickers", choices = symbol_choices, selected = NULL, server = TRUE)
        showNotification(paste("Removed unrecognized:", paste(input$tickers[bad], collapse = ", ")),
                         type = "warning", duration = 5)
        return(invisible(NULL))
      } else {
        showNotification(paste("Removed unrecognized:", paste(input$tickers[bad], collapse = ", ")),
                         type = "warning", duration = 5)
      }
    } else {
      fixed <- corrected
    }
    fixed <- unique(fixed)
    compliance <- filter_ey_compliant_tickers(fixed)
    if (length(compliance$excluded)) {
      showNotification(
        paste("Removed EY-audit restricted tickers:", paste(compliance$excluded, collapse = ", ")),
        type = "warning", duration = 6
      )
    }
    fixed <- compliance$allowed
    if (!identical(sort(fixed), sort(input$tickers))) {
      updateSelectizeInput(session, "tickers", choices = symbol_choices, selected = fixed, server = TRUE)
      if (length(bad) == 0 && !identical(input$tickers, fixed)) {
        showNotification(paste("Auto-corrected to:", paste(fixed, collapse = ", ")),
                         type = "message", duration = 3)
      }
    }
  }, ignoreInit = FALSE, priority = 100)
  
  observeEvent(input$preset, {
    if (input$preset != "None") {
      p <- presets[[input$preset]]
      updateSelectizeInput(session, "tickers", choices = symbol_choices, selected = p$tickers, server = TRUE)
      updateTextAreaInput(session, "weights", value = paste(p$weights, collapse = ", "))
      updateSelectInput(session, "mode", selected = "fixed")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$risk_profile, {
    if (input$risk_profile == "Cautious") {
      updateSelectInput(session, "mode", selected="riskparity")
      updateSliderInput(session, "conf", value=0.975)
      updateSelectInput(session, "rebal", selected="months")
    } else if (input$risk_profile == "Balanced") {
      updateSelectInput(session, "mode", selected="equal")
      updateSliderInput(session, "conf", value=0.95)
      updateSelectInput(session, "rebal", selected="months")
    } else {
      updateSelectInput(session, "mode", selected="momentum")
      updateSliderInput(session, "conf", value=0.90)
      updateSelectInput(session, "rebal", selected="quarters")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$equalize, {
    req(input$tickers)
    n <- length(input$tickers)
    if (n >= 1) {
      eq <- rep(1/n, n)
      updateTextAreaInput(session, "weights", value = paste(eq, collapse = ", "))
    }
  })
  
  results <- eventReactive(input$run, {
    validate(
      need(length(input$tickers) >= 2, "Please add at least 2 tickers."),
      need(!is.null(input$dates[1]) && !is.null(input$dates[2]), "Please choose a valid date range.")
    )
    
    compliance <- filter_ey_compliant_tickers(input$tickers)
    if (length(compliance$excluded)) {
      showNotification(
        paste("Excluded EY-audit restricted tickers:", paste(compliance$excluded, collapse = ", ")), 
        type = "warning", duration = 6
      )
    }
    tickers_use <- compliance$allowed
    validate(need(length(tickers_use) >= 2, "Candidate universe is empty after compliance filters."))

    weights0 <- NULL
    if (input$mode == "fixed") {
      validate(need(nchar(input$weights) > 0, "Enter weights (comma-separated) for fixed mode."))
      w <- as.numeric(strsplit(gsub("\\s", "", input$weights), ",")[[1]])
      validate(
        need(length(w) == length(input$tickers), "Number of weights must match number of tickers."),
        need(abs(sum(w) - 1) < 1e-6, "Weights must sum to 1.")
      )
      named_w <- setNames(w, toupper(input$tickers))
      named_w <- named_w[tickers_use]
      validate(need(length(named_w) == length(tickers_use), "Weights missing for allowed tickers after exclusions."))
      weights0 <- as.numeric(named_w / sum(named_w))
    }
    
    withProgress(message="Fetching prices…", value=0.2, {
      # Robust per-symbol fetch (handles hyphens without relying on auto-assigned objects)
      prices_list <- lapply(tickers_use, function(t) {
        xt <- tryCatch({
          suppressWarnings(
            quantmod::getSymbols(t, from=input$dates[1], to=input$dates[2], auto.assign=FALSE, warnings=FALSE)
          )
        }, error=function(e) NULL)
        if (is.null(xt)) return(NULL)
        Ad(xt)
      })
      ok <- !vapply(prices_list, is.null, logical(1))
      validate(need(any(ok), "Download failed for all symbols. Try a different range or remove delisted tickers."))
      prices_list <- prices_list[ok]
      tick_ok <- tickers_use[ok]
      prices <- do.call(merge, prices_list)
      colnames(prices) <- tick_ok
      
      prices <- na.locf(prices, na.rm=FALSE)
      prices <- prices[complete.cases(prices), ]
      validate(need(NROW(prices) > 260, "Not enough data; try a longer window."))
      
      incProgress(0.3, detail="Building portfolio…")
      params <- list(vol_look = 60, mom_look = 252)
      opt_res <- optimize_portfolio(prices, input$mode, input$rebal, params, weights0)
      r_xts <- opt_res$r_xts
      validate(need(NROW(r_xts) > 0, "No returns computed; check your inputs."))
      r <- as.numeric(r_xts)
      last_w <- if (is.null(opt_res$last_w)) (rep(1/NCOL(prices), NCOL(prices))) else opt_res$last_w
      
      incProgress(0.2, detail="Computing risk metrics…")
      VaR_hist  <- hist_VaR(r, input$conf)
      VaR_param <- param_VaR(r, input$conf)
      VaR_mc    <- mc_VaR(r, n=10000, conf=input$conf)
      VaR_boot  <- boot_VaR(r, B=20000, conf=input$conf)
      ES_hist_  <- hist_ES(r, input$conf)
      
      vals <- c(Historical=VaR_hist, Parametric=VaR_param, `Monte Carlo`=VaR_mc, Bootstrap=VaR_boot, `Hist ES`=ES_hist_)
      risk_tbl <- data.frame(
        Method = names(vals),
        ReturnCutoff = round(as.numeric(vals), 6),
        LossPct = round(-as.numeric(vals) * 100, 3),
        Dollar_at_Notional = round(-as.numeric(vals) * input$notional, 2),
        check.names = FALSE
      )
      
      plots <- make_plots(r_xts, input$conf)
      
      maxdd_val <- suppressWarnings(PerformanceAnalytics::maxDrawdown(r_xts))
      stats <- data.frame(
        Metric = c("Start","End","Obs","CAGR","AnnVol","Sharpe","MaxDD","WorstDay","BestDay","Rebal","Weights"),
        Value  = c(as.character(start(r_xts)),
                   as.character(end(r_xts)),
                   NROW(r_xts),
                   round(cagr(r_xts),4),
                   round(ann_vol(r_xts),4),
                   round(sharpe(r_xts, 0.02),3),
                   round(maxdd_val,4),
                   round(min(r, na.rm=TRUE),4),
                   round(max(r, na.rm=TRUE),4),
                   input$rebal,
                   input$mode),
        stringsAsFactors = FALSE
      )
      rownames(stats) <- stats$Metric

      weights_named <- setNames(last_w, colnames(prices))
      stress_tbl <- simulate_event_impact(
        prices,
        weights_named,
        focus_ticker = tickers_use[1],
        peer_ticker = if (length(tickers_use) >= 2) tail(tickers_use, 1) else tickers_use[1]
      )

      list(
        prices = prices,
        r_xts = r_xts,
        risk_tbl = risk_tbl,
        stats = stats,
        plots = plots,
        last_w = last_w,
        optimizer = list(
          expected_return = opt_res$expected_annual_return,
          expected_vol = opt_res$expected_annual_vol,
          expected_sharpe = opt_res$expected_sharpe,
          weights = opt_res$asset_expected
        ),
        compliance = list(excluded = compliance$excluded, universe = tickers_use),
        stress = stress_tbl
      )
    })
  }, ignoreInit = TRUE)
  
  # Tables
  output$risk_tbl <- renderDT({ req(results()); datatable(results()$risk_tbl, rownames=FALSE, options=list(pageLength=5)) })
  output$stats_tbl <- renderDT({ req(results()); datatable(results()$stats, rownames=FALSE, options=list(dom='t')) })
  output$weights_tbl <- renderDT({
    req(results())
    opt <- results()$optimizer
    validate(need(!is.null(opt$weights), "Run the optimizer to view weights."))
    datatable(opt$weights, rownames = FALSE, options = list(dom = 't', pageLength = 8))
  })
  output$opt_metrics <- renderUI({
    req(results())
    opt <- results()$optimizer
    if (is.null(opt)) return(NULL)
    fmt <- function(x) if (is.na(x)) "—" else sprintf("%0.2f%%", x * 100)
    tags$div(
      tags$p(tags$strong("Expected annual return"), fmt(opt$expected_return)),
      tags$p(tags$strong("Expected annual volatility"), fmt(opt$expected_vol)),
      tags$p(tags$strong("Expected Sharpe"), if (is.na(opt$expected_sharpe)) "—" else sprintf("%0.2f", opt$expected_sharpe))
    )
  })
  output$compliance_ui <- renderUI({
    req(results())
    cmp <- results()$compliance
    if (is.null(cmp)) return(NULL)
    msgs <- list(
      tags$p(tags$strong("Universe"), paste(cmp$universe, collapse = ", "))
    )
    if (length(cmp$excluded)) {
      msgs <- c(msgs, list(tags$p(tags$strong("Restricted"), paste(cmp$excluded, collapse = ", ")), tags$p("Excluded per EY audit policy.")))
    } else {
      msgs <- c(msgs, list(tags$p("No EY audit exclusions triggered.")))
    }
    do.call(tags$div, msgs)
  })
  output$stress_tbl <- renderDT({
    req(results())
    st <- results()$stress
    if (is.null(st) || !NROW(st)) return(NULL)
    datatable(st, rownames = FALSE, options = list(dom = 't', pageLength = 5))
  })

  # Plots
  output$p_hist <- renderPlot({ req(results()); results()$plots$p_hist })
  output$p_roll <- renderPlot({ req(results()); results()$plots$p_roll })
  output$p_eq   <- renderPlot({ req(results()); results()$plots$p_eq })
  output$p_dd   <- renderPlot({ req(results()); results()$plots$p_dd })
  
  # Grade, labels, checks, suggestions
  output$grade_score <- renderText({
    req(results())
    w <- results()$last_w; if (is.null(w)) w <- rep(1/NCOL(results()$prices), NCOL(results()$prices))
    vals <- setNames(results()$risk_tbl$ReturnCutoff, results()$risk_tbl$Method)
    s <- score_grade(results()$stats, vals, w, input$conf); sprintf("%d / 100", s)
  })
  
  output$grade_label <- renderText({
    req(results())
    rp <- results()$stats
    vol     <- num_or(rp["AnnVol","Value"])
    var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method=="Historical"])
    if (!is.finite(vol) || !is.finite(var_pct)) return("—")
    risk_label(vol, var_pct)
  })
  
  output$checks_ui <- renderUI({
    req(results())
    w <- results()$last_w; if (is.null(w) || !length(w)) return(NULL)
    
    rp <- results()$stats
    vol     <- num_or(rp["AnnVol","Value"])
    maxdd   <- num_or(rp["MaxDD","Value"])
    var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method=="Historical"])
    
    if (any(!is.finite(c(vol, maxdd, var_pct)))) return(NULL)
    
    items <- traffic_checks(w, vol, var_pct, maxdd, length(w))
    tags$ul(lapply(items, tags$li))
  })
  
  output$suggestions_ui <- renderUI({
    req(results())
    w <- results()$last_w; if (is.null(w) || !length(w)) {
      return(tags$p("Run analysis or adjust your date range to see suggestions."))
    }
    
    rp <- results()$stats
    vol     <- num_or(rp["AnnVol","Value"])
    maxdd   <- num_or(rp["MaxDD","Value"])
    var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method=="Historical"])
    
    if (any(!is.finite(c(vol, maxdd, var_pct)))) {
      return(tags$p("Run analysis or adjust your date range to see suggestions."))
    }
    
    sugs <- suggestions(w, length(w), vol, var_pct, maxdd, input$mode)
    if (is.character(sugs)) tags$p(sugs) else tags$ul(lapply(sugs, tags$li))
  })
  
  # PDF download
  output$download_pdf <- downloadHandler(
    filename = function() sprintf("portfolio_report_%s.pdf", Sys.Date()),
    content = function(file) {
      req(results())
      r_xts <- results()$r_xts; plots <- results()$plots
      risk_tbl <- results()$risk_tbl; stats <- results()$stats
      tickers <- colnames(results()$prices)
      
      pdf(file, width=11, height=8.5)
      grid.newpage()
      title <- textGrob("Portfolio Risk One-Pager", gp=gpar(fontsize=18, fontface="bold"))
      sub   <- textGrob(
        paste0("Universe: ", paste(tickers, collapse=", "),
               "   |   Weighting: ", input$mode,
               "   |   Rebal: ", input$rebal,
               "   |   Start: ", as.character(start(r_xts)),
               "   |   End: ", as.character(end(r_xts))),
        gp=gpar(fontsize=10)
      )
      grid.arrange(title, sub, nrow=2, heights=c(0.6,0.4))
      grid.arrange(tableGrob(stats, rows=NULL), tableGrob(risk_tbl, rows=NULL), ncol=2)
      grid.arrange(plots$p_hist, plots$p_roll, ncol=2)
      grid.arrange(plots$p_eq, plots$p_dd, ncol=2)
      dev.off()
    }
  )
}

shinyApp(ui, server)

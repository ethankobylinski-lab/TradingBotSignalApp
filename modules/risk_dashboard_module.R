risk_dashboard_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("risk_tickers"), "Coverage", choices = symbol_choices, selected = c("AAPL", "MSFT", "GOOGL", "AMZN"), multiple = TRUE, options = list(create = TRUE, plugins = list("remove_button"))),
      dateRangeInput(ns("risk_range"), "Lookback", start = Sys.Date() - 365 * 3, end = Sys.Date()),
      checkboxGroupInput(ns("risk_metrics"), "Metrics", choices = c("Annual Vol" = "vol", "Hist VaR" = "var", "Max Drawdown" = "dd", "Beta vs SPY" = "beta", "Momentum" = "mom"), selected = c("vol", "var", "beta", "mom")),
      actionButton(ns("risk_refresh"), "Build dashboard", class = "btn-primary")
    ),
    tagList(
      fluidRow(
        column(6,
               card(
                 card_header("Factor heatmap"),
                 plotOutput(ns("risk_heatmap"), height = 320)
               )
        ),
        column(6,
               card(
                 card_header("Beta vs momentum"),
                 plotOutput(ns("risk_scatter"), height = 320)
               )
        )
      ),
      fluidRow(
        column(12,
               card(
                 card_header("Risk table"),
                 DTOutput(ns("risk_table"))
               )
        )
      )
    )
  )
}

risk_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "risk_tickers", choices = symbol_choices, server = TRUE)

    risk_data <- eventReactive(input$risk_refresh, {
      validate(need(length(input$risk_tickers) >= 2, "Select at least two tickers."))
      rng <- input$risk_range
      validate(need(!is.null(rng[1]) && !is.null(rng[2]), "Choose a valid date range."))
      tickers <- unique(vapply(input$risk_tickers, resolve_to_symbol, character(1)))
      tickers <- tickers[!is.na(tickers)]
      validate(need(length(tickers) >= 1, "No valid tickers."))

      withProgress(message = "Fetching universe", value = 0.2, {
        px_list <- lapply(tickers, function(t) {
          tryCatch({
            suppressWarnings(quantmod::getSymbols(t, from = rng[1], to = rng[2], auto.assign = FALSE, warnings = FALSE))
          }, error = function(e) NULL)
        })
        ok <- !vapply(px_list, is.null, logical(1))
        validate(need(any(ok), "Failed to download data."))
        px_list <- px_list[ok]
        tickers_ok <- tickers[ok]
        merged <- do.call(merge, lapply(px_list, Ad))
        colnames(merged) <- tickers_ok
        merged <- na.locf(merged, na.rm = FALSE)
        merged <- merged[complete.cases(merged), ]
        validate(need(NROW(merged) > 150, "Not enough data."))

        returns <- na.omit(diff(log(merged)))
        spy <- tryCatch({
          suppressWarnings(quantmod::getSymbols("SPY", from = rng[1], to = rng[2], auto.assign = FALSE, warnings = FALSE))
        }, error = function(e) NULL)
        spy_ret <- if (!is.null(spy)) na.omit(diff(log(Ad(spy)))) else NULL

        metrics <- lapply(seq_along(tickers_ok), function(i) {
          tk <- tickers_ok[i]
          r <- returns[, i]
          px <- merged[, i]
          vol <- ann_vol(r)
          var <- hist_VaR(as.numeric(r), 0.95)
          dd <- suppressWarnings(PerformanceAnalytics::maxDrawdown(r))
          mom <- if (NROW(px) > 126) as.numeric(last(px) / px[NROW(px) - 125] - 1) else NA_real_
          beta <- NA_real_
          if (!is.null(spy_ret)) {
            common <- merge(r, spy_ret, join = "inner")
            if (NROW(common) > 50) {
              beta <- cov(common[,1], common[,2], use = "complete.obs") / var(common[,2], na.rm = TRUE)
            }
          }
          data.frame(Ticker = tk, vol = vol, var = -var * 100, dd = dd, beta = beta, mom = mom, stringsAsFactors = FALSE)
        })
        metrics_df <- do.call(rbind, metrics)
        list(metrics = metrics_df)
      })
    }, ignoreNULL = FALSE)

    output$risk_heatmap <- renderPlot({
      dat <- risk_data(); req(nrow(dat$metrics) > 0)
      selected <- input$risk_metrics
      if (!length(selected)) return(NULL)
      df <- dat$metrics[, c("Ticker", selected), drop = FALSE]
      df <- tidyr::pivot_longer(df, cols = -Ticker, names_to = "metric", values_to = "value")
      ggplot(df, aes(metric, Ticker, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#27AE60", mid = "white", high = "#C0392B", midpoint = median(df$value, na.rm = TRUE), na.value = "grey90") +
        labs(x = NULL, y = NULL, fill = "Score", title = "Risk factor heatmap") +
        theme_minimal()
    })

    output$risk_scatter <- renderPlot({
      dat <- risk_data(); req(nrow(dat$metrics) > 0)
      df <- dat$metrics
      ggplot(df, aes(mom, beta, size = vol, label = Ticker)) +
        geom_point(color = "#2C3E50", alpha = 0.7) +
        geom_text(vjust = -0.8, size = 3, show.legend = FALSE) +
        labs(x = "Momentum (126d)", y = "Beta vs SPY", size = "Ann Vol", title = "Momentum vs Beta") +
        theme_minimal()
    })

    output$risk_table <- renderDT({
      dat <- risk_data(); req(nrow(dat$metrics) > 0)
      df <- dat$metrics
      df$`Annual Vol` <- scales::percent(df$vol, accuracy = 0.1)
      df$`Hist VaR (95%)` <- paste0(format(round(df$var, 2), nsmall = 2), "%")
      df$`Max DD` <- scales::percent(df$dd, accuracy = 0.1)
      df$`Beta vs SPY` <- round(df$beta, 2)
      df$`Momentum 126d` <- scales::percent(df$mom, accuracy = 0.1)
      tbl <- df[, c("Ticker", "Annual Vol", "Hist VaR (95%)", "Max DD", "Beta vs SPY", "Momentum 126d")]
      datatable(tbl, rownames = FALSE, options = list(dom = 't'))
    })
  })
}

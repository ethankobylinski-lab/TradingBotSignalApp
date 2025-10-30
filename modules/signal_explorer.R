signal_explorer_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("ticker"), "Primary ticker", choices = symbol_choices, selected = "AAPL", options = list(create = TRUE)),
      dateRangeInput(ns("range"), "History", start = Sys.Date() - 365 * 3, end = Sys.Date()),
      checkboxGroupInput(ns("signals"), "Signals", choices = c("SMA 50" = "sma50", "SMA 200" = "sma200", "Bollinger Bands" = "bbands", "RSI (14)" = "rsi", "MACD" = "macd"),
                         selected = c("sma50", "sma200")),
      checkboxInput(ns("compare_spy"), "Compare to S&P 500 (SPY)", value = TRUE),
      actionButton(ns("refresh"), "Update Explorer", class = "btn-primary")
    ),
    tagList(
      fluidRow(
        column(6,
               card(
                 card_header("Price & overlays"),
                 plotOutput(ns("price_plot"), height = 320)
               )
        ),
        column(6,
               card(
                 card_header("Backtest vs benchmark"),
                 plotOutput(ns("backtest_plot"), height = 320)
               )
        )
      ),
      fluidRow(
        column(6,
               card(
                 card_header("Signal details"),
                 plotOutput(ns("indicator_plot"), height = 280)
               )
        ),
        column(6,
               card(
                 card_header("Exploration metrics"),
                 DTOutput(ns("metric_tbl"))
               )
        )
      )
    )
  )
}

signal_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "ticker", choices = symbol_choices, server = TRUE)

    explorer_data <- eventReactive(input$refresh, {
      validate(need(!is.null(input$ticker) && nzchar(input$ticker), "Select a ticker to explore."))
      rng <- input$range
      validate(need(!is.null(rng[1]) && !is.null(rng[2]), "Choose a valid date range."))

      sym <- resolve_to_symbol(input$ticker)
      validate(need(!is.na(sym), "Unable to resolve ticker."))

      withProgress(message = sprintf("Loading %s", sym), value = 0.2, {
        px <- tryCatch({
          suppressWarnings(
            quantmod::getSymbols(sym, from = rng[1], to = rng[2], auto.assign = FALSE, warnings = FALSE)
          )
        }, error = function(e) NULL)
        validate(need(!is.null(px), "Unable to download ticker history."))
        price <- Ad(px)
        price <- na.locf(price, na.rm = FALSE)
        price <- price[complete.cases(price), ]
        validate(need(NROW(price) > 120, "Need at least 120 observations."))

        bench <- NULL
        if (isTRUE(input$compare_spy)) {
          bench <- tryCatch({
            suppressWarnings(
              quantmod::getSymbols("SPY", from = rng[1], to = rng[2], auto.assign = FALSE, warnings = FALSE)
            )
          }, error = function(e) NULL)
          if (!is.null(bench)) {
            bench <- Ad(bench)
            bench <- na.locf(bench, na.rm = FALSE)
            bench <- bench[complete.cases(bench), ]
          }
        }

        sma50  <- if ("sma50" %in% input$signals) stats::filter(price, rep(1/50, 50), sides = 1) else NULL
        sma200 <- if ("sma200" %in% input$signals) stats::filter(price, rep(1/200, 200), sides = 1) else NULL
        bb     <- if ("bbands" %in% input$signals) suppressWarnings(quantmod::BBands(price)) else NULL
        rsi    <- if ("rsi" %in% input$signals) suppressWarnings(quantmod::RSI(price)) else NULL
        macd   <- if ("macd" %in% input$signals) suppressWarnings(quantmod::MACD(price, 12, 26, 9)) else NULL

        price_df <- data.frame(date = index(price), price = as.numeric(price))
        if (!is.null(sma50)) price_df$sma50 <- as.numeric(sma50)
        if (!is.null(sma200)) price_df$sma200 <- as.numeric(sma200)
        if (!is.null(bb)) {
          price_df$bb_up <- as.numeric(bb$up)
          price_df$bb_dn <- as.numeric(bb$dn)
        }

        indicators <- list(rsi = rsi, macd = macd)

        returns <- na.omit(diff(log(price)))
        bench_returns <- if (!is.null(bench)) na.omit(diff(log(bench))) else NULL

        list(symbol = sym, price = price, price_df = price_df, indicators = indicators,
             returns = returns, bench_returns = bench_returns)
      })
    }, ignoreNULL = FALSE)

    output$price_plot <- renderPlot({
      dat <- explorer_data(); req(nrow(dat$price_df) > 0)
      df <- dat$price_df
      p <- ggplot(df, aes(date, price)) +
        geom_line(color = "#2C3E50", linewidth = 0.9) +
        labs(title = paste(dat$symbol, "price"), y = "Adj Close", x = NULL) +
        theme_minimal()
      if ("sma50" %in% input$signals && "sma50" %in% names(df)) {
        p <- p + geom_line(aes(y = sma50), color = "#18BC9C", linewidth = 0.6)
      }
      if ("sma200" %in% input$signals && "sma200" %in% names(df)) {
        p <- p + geom_line(aes(y = sma200), color = "#E74C3C", linewidth = 0.6)
      }
      if ("bbands" %in% input$signals && all(c("bb_up", "bb_dn") %in% names(df))) {
        p <- p + geom_ribbon(aes(ymin = bb_dn, ymax = bb_up), fill = "#3498DB", alpha = 0.15)
      }
      p
    })

    output$backtest_plot <- renderPlot({
      dat <- explorer_data(); req(NROW(dat$returns) > 0)
      eq <- xts(exp(cumsum(dat$returns)), order.by = index(dat$returns))
      plot_df <- data.frame(date = index(eq), strategy = as.numeric(eq), stringsAsFactors = FALSE)
      if (!is.null(dat$bench_returns)) {
        bench_eq <- xts(exp(cumsum(dat$bench_returns)), order.by = index(dat$bench_returns))
        plot_df$benchmark <- as.numeric(bench_eq[match(plot_df$date, index(bench_eq))])
      }
      plot_df <- tidyr::pivot_longer(plot_df, cols = -date, names_to = "series", values_to = "value")
      ggplot(plot_df, aes(date, value, color = series)) +
        geom_line(linewidth = 0.9) +
        scale_color_manual(values = c("strategy" = "#2C3E50", "benchmark" = "#E67E22"), labels = c("strategy" = "Strategy", "benchmark" = "S&P 500")) +
        labs(title = "Growth of $1", x = NULL, y = "Value", color = NULL) +
        theme_minimal()
    })

    output$indicator_plot <- renderPlot({
      dat <- explorer_data()
      df <- dat$price_df
      plots <- list()
      if ("rsi" %in% input$signals && !is.null(dat$indicators$rsi)) {
        rsi_df <- data.frame(date = index(dat$indicators$rsi), value = as.numeric(dat$indicators$rsi))
        plots[["rsi"]] <- ggplot(rsi_df, aes(date, value)) +
          geom_line(color = "#8E44AD") +
          geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "#95A5A6") +
          labs(title = "RSI (14)", x = NULL, y = "RSI") +
          theme_minimal()
      }
      if ("macd" %in% input$signals && !is.null(dat$indicators$macd)) {
        macd_df <- data.frame(date = index(dat$indicators$macd),
                              macd = as.numeric(dat$indicators$macd[, "macd"]),
                              signal = as.numeric(dat$indicators$macd[, "signal"]))
        plots[["macd"]] <- ggplot(macd_df, aes(date, macd)) +
          geom_line(color = "#16A085") +
          geom_line(aes(y = signal), color = "#C0392B", linewidth = 0.7) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D") +
          labs(title = "MACD", x = NULL, y = NULL) +
          theme_minimal()
      }
      if (!length(plots)) {
        ggplot(df, aes(date, price)) + geom_line(color = "#2C3E50") + theme_minimal() + labs(title = "No indicators selected", x = NULL, y = NULL)
      } else if (length(plots) == 1) {
        plots[[1]]
      } else {
        gridExtra::grid.arrange(grobs = lapply(plots, ggplotGrob), ncol = 1)
      }
    })

    output$metric_tbl <- renderDT({
      dat <- explorer_data(); req(NROW(dat$returns) > 0)
      stats <- data.frame(
        Metric = c("CAGR", "Ann. Vol", "Sharpe (2%)", "Max Drawdown"),
        Value = c(
          scales::percent(cagr(dat$returns), accuracy = 0.1),
          scales::percent(ann_vol(dat$returns), accuracy = 0.1),
          round(sharpe(dat$returns, 0.02), 2),
          scales::percent(PerformanceAnalytics::maxDrawdown(dat$returns), accuracy = 0.1)
        )
      )
      if (!is.null(dat$bench_returns)) {
        te <- sd(dat$returns - dat$bench_returns, na.rm = TRUE) * sqrt(252)
        stats <- rbind(stats, data.frame(Metric = "Tracking error", Value = scales::percent(te, accuracy = 0.1)))
        ir <- (mean(dat$returns, na.rm = TRUE) - mean(dat$bench_returns, na.rm = TRUE)) / sd(dat$returns - dat$bench_returns, na.rm = TRUE)
        stats <- rbind(stats, data.frame(Metric = "Information ratio", Value = round(ir * sqrt(252), 2)))
      }
      datatable(stats, rownames = FALSE, options = list(dom = 't'))
    })
  })
}

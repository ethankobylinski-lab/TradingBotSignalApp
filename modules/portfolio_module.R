portfolio_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 360,
      h4("Quick start"),
      radioButtons(ns("preset"), NULL, choices = c("None", names(presets)), inline = FALSE, selected = "None"),
      h4("Build your portfolio"),
      selectizeInput(
        ns("tickers"), "Tickers",
        choices = symbol_choices,
        multiple = TRUE,
        options = list(
          create = TRUE,
          persist = TRUE,
          selectOnTab = TRUE,
          createOnBlur = TRUE,
          placeholder = "Type company or symbol… (e.g., apple → AAPL)",
          plugins = list("remove_button"),
          render = I("{\n            option: function(item, escape) { return '<div>' + escape(item.label) + '</div>'; },\n            item:   function(item, escape) { return '<div>' + escape(item.label) + '</div>'; }\n          }")
        )
      ),
      radioButtons(ns("risk_profile"), "Risk profile", choices = c("Cautious", "Balanced", "Aggressive"), inline = TRUE, selected = "Balanced"),
      dateRangeInput(ns("dates"), "Date range", start = "2020-01-01", end = Sys.Date()),
      sliderInput(ns("conf"), "VaR/ES confidence", min = 0.80, max = 0.995, value = 0.95, step = 0.005),
      numericInput(ns("notional"), "Notional ($)", 100000, min = 1000, step = 1000),
      selectInput(ns("rebal"), "Rebalance", c("none", "months", "quarters"), selected = "months"),
      selectInput(ns("mode"), "Weighting", c("fixed", "equal", "riskparity", "momentum"), selected = "equal"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'fixed'", ns("mode")),
        textAreaInput(ns("weights"), "Fixed weights (comma-separated; must sum to 1)", rows = 3,
                      placeholder = "e.g. 0.2, 0.2, 0.2, 0.2, 0.2"),
        actionButton(ns("equalize"), "Equalize weights for selected tickers")
      ),
      hr(),
      actionButton(ns("run"), "Run analysis", class = "btn-primary"),
      br(), br(),
      downloadButton(ns("download_pdf"), "Download PDF")
    ),
    tagList(
      fluidRow(
        column(
          4,
          card(
            card_header("Portfolio Grade"),
            h1(textOutput(ns("grade_score")), style = "margin-top:0"),
            tags$div(textOutput(ns("grade_label")), style = "font-weight:600;margin-bottom:8px;"),
            tags$small("Grade blends Sharpe, VaR, max drawdown, and concentration (HHI).")
          )
        ),
        column(
          8,
          card(
            card_header("Traffic-light checks"),
            uiOutput(ns("checks_ui")),
            card(
              card_header("Suggestions"),
              uiOutput(ns("suggestions_ui"))
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          card(
            card_header("Snapshot metrics"),
            uiOutput(ns("metric_boxes"))
          )
        ),
        column(
          6,
          card(
            card_header("Recommended portfolios"),
            DTOutput(ns("recommended_tbl"))
          )
        )
      ),
      card(
        card_header("Risk Summary"),
        fluidRow(
          column(6, DTOutput(ns("risk_tbl"))),
          column(6, DTOutput(ns("stats_tbl")))
        )
      ),
      card(
        card_header("Distributions & Risk Over Time"),
        fluidRow(
          column(6, plotOutput(ns("p_hist"), height = 300)),
          column(6, plotOutput(ns("p_roll"), height = 300))
        )
      ),
      card(
        card_header("Growth & Drawdowns"),
        fluidRow(
          column(6, plotOutput(ns("p_eq"), height = 300)),
          column(6, plotOutput(ns("p_dd"), height = 300))
        )
      )
    )
  )
}

portfolio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "tickers", choices = symbol_choices, server = TRUE, selected = NULL)

    observeEvent(input$tickers, {
      req(input$tickers)
      corrected <- vapply(input$tickers, resolve_to_symbol, character(1))
      bad <- which(is.na(corrected))
      if (length(bad)) {
        fixed <- corrected[!is.na(corrected)]
        if (length(fixed) == 0) {
          updateSelectizeInput(session, "tickers", choices = symbol_choices, selected = NULL, server = TRUE)
          showNotification(paste("Removed unrecognized:", paste(input$tickers[bad], collapse = ", ")), type = "warning", duration = 5)
          return(invisible(NULL))
        } else {
          showNotification(paste("Removed unrecognized:", paste(input$tickers[bad], collapse = ", ")), type = "warning", duration = 5)
        }
      } else {
        fixed <- corrected
      }
      fixed <- unique(fixed)
      if (!identical(sort(fixed), sort(input$tickers))) {
        updateSelectizeInput(session, "tickers", choices = symbol_choices, selected = fixed, server = TRUE)
        if (length(bad) == 0 && !identical(input$tickers, fixed)) {
          showNotification(paste("Auto-corrected to:", paste(fixed, collapse = ", ")), type = "message", duration = 3)
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
        updateSelectInput(session, "mode", selected = "riskparity")
        updateSliderInput(session, "conf", value = 0.975)
        updateSelectInput(session, "rebal", selected = "months")
      } else if (input$risk_profile == "Balanced") {
        updateSelectInput(session, "mode", selected = "equal")
        updateSliderInput(session, "conf", value = 0.95)
        updateSelectInput(session, "rebal", selected = "months")
      } else {
        updateSelectInput(session, "mode", selected = "momentum")
        updateSliderInput(session, "conf", value = 0.90)
        updateSelectInput(session, "rebal", selected = "quarters")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$equalize, {
      req(input$tickers)
      n <- length(input$tickers)
      if (n >= 1) {
        eq <- rep(1 / n, n)
        updateTextAreaInput(session, "weights", value = paste(eq, collapse = ", "))
      }
    })

    results <- eventReactive(input$run, {
      validate(
        need(length(input$tickers) >= 2, "Please add at least 2 tickers."),
        need(!is.null(input$dates[1]) && !is.null(input$dates[2]), "Please choose a valid date range.")
      )

      weights0 <- NULL
      if (input$mode == "fixed") {
        validate(need(nchar(input$weights) > 0, "Enter weights (comma-separated) for fixed mode."))
        w <- as.numeric(strsplit(gsub("\\\s", "", input$weights), ",")[[1]])
        validate(
          need(length(w) == length(input$tickers), "Number of weights must match number of tickers."),
          need(abs(sum(w) - 1) < 1e-6, "Weights must sum to 1.")
        )
        weights0 <- w
      }

      withProgress(message = "Fetching prices…", value = 0.2, {
        prices_list <- lapply(input$tickers, function(t) {
          xt <- tryCatch({
            suppressWarnings(
              quantmod::getSymbols(t, from = input$dates[1], to = input$dates[2], auto.assign = FALSE, warnings = FALSE)
            )
          }, error = function(e) NULL)
          if (is.null(xt)) return(NULL)
          Ad(xt)
        })
        ok <- !vapply(prices_list, is.null, logical(1))
        validate(need(any(ok), "Download failed for all symbols. Try a different range or remove delisted tickers."))
        prices_list <- prices_list[ok]
        tick_ok <- input$tickers[ok]
        prices <- do.call(merge, prices_list)
        colnames(prices) <- tick_ok

        prices <- na.locf(prices, na.rm = FALSE)
        prices <- prices[complete.cases(prices), ]
        validate(need(NROW(prices) > 260, "Not enough data; try a longer window."))

        incProgress(0.3, detail = "Building portfolio…")
        built <- build_port(prices, input$rebal, input$mode, 60, 252, weights0)
        r_xts <- built$r_xts
        validate(need(NROW(r_xts) > 0, "No returns computed; check your inputs."))
        r <- as.numeric(r_xts)
        last_w <- if (is.null(built$last_w)) (rep(1 / NCOL(prices), NCOL(prices))) else built$last_w

        incProgress(0.2, detail = "Computing risk metrics…")
        VaR_hist  <- hist_VaR(r, input$conf)
        VaR_param <- param_VaR(r, input$conf)
        VaR_mc    <- mc_VaR(r, n = 10000, conf = input$conf)
        VaR_boot  <- boot_VaR(r, B = 20000, conf = input$conf)
        ES_hist_  <- hist_ES(r, input$conf)

        vals <- c(Historical = VaR_hist, Parametric = VaR_param, `Monte Carlo` = VaR_mc, Bootstrap = VaR_boot, `Hist ES` = ES_hist_)
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
          Metric = c("Start", "End", "Obs", "CAGR", "AnnVol", "Sharpe", "MaxDD", "WorstDay", "BestDay", "Rebal", "Weights"),
          Value  = c(as.character(start(r_xts)),
                     as.character(end(r_xts)),
                     NROW(r_xts),
                     round(cagr(r_xts), 4),
                     round(ann_vol(r_xts), 4),
                     round(sharpe(r_xts, 0.02), 3),
                     round(maxdd_val, 4),
                     round(min(r, na.rm = TRUE), 4),
                     round(max(r, na.rm = TRUE), 4),
                     input$rebal,
                     input$mode),
          stringsAsFactors = FALSE
        )
        rownames(stats) <- stats$Metric

        list(prices = prices, r_xts = r_xts, risk_tbl = risk_tbl, stats = stats, plots = plots, last_w = last_w)
      })
    }, ignoreNULL = TRUE)

    output$risk_tbl <- renderDT({ req(results()); datatable(results()$risk_tbl, rownames = FALSE, options = list(pageLength = 5)) })
    output$stats_tbl <- renderDT({ req(results()); datatable(results()$stats, rownames = FALSE, options = list(dom = 't')) })

    output$p_hist <- renderPlot({ req(results()); results()$plots$p_hist })
    output$p_roll <- renderPlot({ req(results()); results()$plots$p_roll })
    output$p_eq   <- renderPlot({ req(results()); results()$plots$p_eq })
    output$p_dd   <- renderPlot({ req(results()); results()$plots$p_dd })

    output$grade_score <- renderText({
      req(results())
      w <- results()$last_w; if (is.null(w)) w <- rep(1 / NCOL(results()$prices), NCOL(results()$prices))
      vals <- setNames(results()$risk_tbl$ReturnCutoff, results()$risk_tbl$Method)
      s <- score_grade(results()$stats, vals, w, input$conf); sprintf("%d / 100", s)
    })

    output$grade_label <- renderText({
      req(results())
      rp <- results()$stats
      vol     <- num_or(rp["AnnVol", "Value"])
      var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method == "Historical"])
      if (!is.finite(vol) || !is.finite(var_pct)) return("—")
      risk_label(vol, var_pct)
    })

    output$checks_ui <- renderUI({
      req(results())
      w <- results()$last_w; if (is.null(w) || !length(w)) return(NULL)

      rp <- results()$stats
      vol     <- num_or(rp["AnnVol", "Value"])
      maxdd   <- num_or(rp["MaxDD", "Value"])
      var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method == "Historical"])

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
      vol     <- num_or(rp["AnnVol", "Value"])
      maxdd   <- num_or(rp["MaxDD", "Value"])
      var_pct <- num_or(results()$risk_tbl$LossPct[results()$risk_tbl$Method == "Historical"])

      if (any(!is.finite(c(vol, maxdd, var_pct)))) {
        return(tags$p("Run analysis or adjust your date range to see suggestions."))
      }

      sugs <- suggestions(w, length(w), vol, var_pct, maxdd, input$mode)
      if (is.character(sugs)) tags$p(sugs) else tags$ul(lapply(sugs, tags$li))
    })

    output$metric_boxes <- renderUI({
      req(results())
      stats <- results()$stats
      risk_tbl <- results()$risk_tbl
      hist_var <- risk_tbl$LossPct[risk_tbl$Method == "Historical"]
      hist_var <- if (length(hist_var)) hist_var[1] else NA
      layout_columns(
        value_box("CAGR", scales::percent(num_or(stats["CAGR", "Value"]), accuracy = 0.1), showcase = icon("chart-line")),
        value_box("Ann. Vol", scales::percent(num_or(stats["AnnVol", "Value"]), accuracy = 0.1), showcase = icon("wave-square")),
        value_box("Hist VaR", if (is.na(hist_var)) "—" else paste0(format(round(hist_var, 2), nsmall = 2), "%"), showcase = icon("shield"))
      )
    })

    output$recommended_tbl <- renderDT({
      datatable(recommended_portfolios, rownames = FALSE, options = list(dom = 't', pageLength = 5), escape = FALSE)
    })

    output$download_pdf <- downloadHandler(
      filename = function() sprintf("portfolio_report_%s.pdf", Sys.Date()),
      content = function(file) {
        req(results())
        r_xts <- results()$r_xts; plots <- results()$plots
        risk_tbl <- results()$risk_tbl; stats <- results()$stats
        tickers <- colnames(results()$prices)

        pdf(file, width = 11, height = 8.5)
        grid.newpage()
        title <- textGrob("Portfolio Risk One-Pager", gp = gpar(fontsize = 18, fontface = "bold"))
        sub   <- textGrob(
          paste0("Universe: ", paste(tickers, collapse = ", "),
                 "   |   Weighting: ", input$mode,
                 "   |   Rebal: ", input$rebal,
                 "   |   Start: ", as.character(start(r_xts)),
                 "   |   End: ", as.character(end(r_xts))),
          gp = gpar(fontsize = 10)
        )
        grid.arrange(title, sub, nrow = 2, heights = c(0.6, 0.4))
        grid.arrange(tableGrob(stats, rows = NULL), tableGrob(risk_tbl, rows = NULL), ncol = 2)
        grid.arrange(plots$p_hist, plots$p_roll, ncol = 2)
        grid.arrange(plots$p_eq, plots$p_dd, ncol = 2)
        dev.off()
      }
    )
  })
}

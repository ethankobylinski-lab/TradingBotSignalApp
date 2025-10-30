signal_lab_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("lab_tickers"), "Universe", choices = symbol_choices, selected = c("AAPL", "MSFT", "NVDA"), multiple = TRUE, options = list(create = TRUE, plugins = list("remove_button"))),
      dateRangeInput(ns("lab_range"), "Window", start = Sys.Date() - 365 * 3, end = Sys.Date()),
      sliderInput(ns("lab_roll"), "Rolling window (days)", min = 60, max = 252, value = 126, step = 6),
      actionButton(ns("lab_refresh"), "Analyze relationships", class = "btn-primary")
    ),
    tagList(
      fluidRow(
        column(6,
               card(
                 card_header("Correlation heatmap"),
                 plotOutput(ns("corr_heatmap"), height = 320)
               )
        ),
        column(6,
               card(
                 card_header("Rolling correlation"),
                 plotOutput(ns("rolling_corr"), height = 320)
               )
        )
      ),
      fluidRow(
        column(6,
               card(
                 card_header("Pair scatter"),
                 plotOutput(ns("pair_scatter"), height = 320)
               )
        ),
        column(6,
               card(
                 card_header("Correlation table"),
                 DTOutput(ns("corr_table"))
               )
        )
      )
    )
  )
}

signal_lab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "lab_tickers", choices = symbol_choices, server = TRUE)

    lab_data <- eventReactive(input$lab_refresh, {
      validate(need(length(input$lab_tickers) >= 2, "Select at least two tickers."))
      rng <- input$lab_range
      validate(need(!is.null(rng[1]) && !is.null(rng[2]), "Choose a valid date range."))

      tickers <- vapply(input$lab_tickers, resolve_to_symbol, character(1))
      tickers <- unique(tickers[!is.na(tickers)])
      validate(need(length(tickers) >= 2, "Need at least two valid tickers."))

      withProgress(message = "Downloading prices", value = 0.2, {
        px_list <- lapply(tickers, function(t) {
          tryCatch({
            suppressWarnings(quantmod::getSymbols(t, from = rng[1], to = rng[2], auto.assign = FALSE, warnings = FALSE))
          }, error = function(e) NULL)
        })
        ok <- !vapply(px_list, is.null, logical(1))
        validate(need(any(ok), "Failed to download prices."))
        px_list <- px_list[ok]
        tickers_ok <- tickers[ok]
        merged <- do.call(merge, lapply(px_list, Ad))
        colnames(merged) <- tickers_ok
        merged <- na.locf(merged, na.rm = FALSE)
        merged <- merged[complete.cases(merged), ]
        validate(need(NROW(merged) > input$lab_roll + 10, "Increase your window; not enough data."))

        returns <- na.omit(diff(log(merged)))
        list(tickers = tickers_ok, prices = merged, returns = returns)
      })
    }, ignoreNULL = FALSE)

    output$corr_heatmap <- renderPlot({
      dat <- lab_data(); req(NCOL(dat$returns) >= 2)
      corr <- cor(dat$returns, use = "pairwise.complete.obs")
      corr_df <- as.data.frame(as.table(corr))
      ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#C0392B", high = "#27AE60", mid = "white", limits = c(-1, 1), oob = scales::squish) +
        labs(x = NULL, y = NULL, fill = "Corr", title = "Correlation matrix") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$rolling_corr <- renderPlot({
      dat <- lab_data(); req(NCOL(dat$returns) >= 2)
      pairs <- combn(colnames(dat$returns), 2)
      window <- input$lab_roll
      rc_list <- lapply(seq_len(ncol(pairs)), function(i) {
        a <- pairs[1, i]; b <- pairs[2, i]
        roll <- tryCatch({
          zoo::rollapply(dat$returns[, c(a, b)], width = window, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right")
        }, error = function(e) NULL)
        if (is.null(roll)) return(NULL)
        data.frame(date = index(roll), value = as.numeric(roll), pair = paste(a, "vs", b), stringsAsFactors = FALSE)
      })
      rc_df <- do.call(rbind, rc_list)
      req(NROW(rc_df) > 0)
      ggplot(rc_df, aes(date, value, color = pair)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D") +
        labs(title = sprintf("%d-day rolling correlations", input$lab_roll), x = NULL, y = "Correlation", color = NULL) +
        theme_minimal()
    })

    output$pair_scatter <- renderPlot({
      dat <- lab_data(); req(NCOL(dat$returns) >= 2)
      cols <- colnames(dat$returns)
      pair <- cols[1:2]
      scatter <- data.frame(x = dat$returns[, pair[1]], y = dat$returns[, pair[2]])
      ggplot(scatter, aes(x, y)) +
        geom_point(alpha = 0.4, color = "#2980B9") +
        geom_smooth(method = "lm", se = FALSE, color = "#E67E22") +
        labs(title = paste(pair[1], "vs", pair[2]), x = paste(pair[1], "returns"), y = paste(pair[2], "returns")) +
        theme_minimal()
    })

    output$corr_table <- renderDT({
      dat <- lab_data(); req(NCOL(dat$returns) >= 2)
      corr <- cor(dat$returns, use = "pairwise.complete.obs")
      datatable(round(corr, 3), options = list(dom = 't'))
    })
  })
}

compliance_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("exclusion_symbol"), "Add EY exclusion", choices = symbol_choices, selected = NULL, multiple = FALSE, options = list(create = TRUE)),
      selectInput(ns("exclusion_reason"), "Reason", choices = c("Sanctions", "Liquidity", "Regulatory", "Client directive")),
      textAreaInput(ns("exclusion_notes"), "Notes", rows = 3, placeholder = "Add supporting details for audit trail"),
      actionButton(ns("add_exclusion"), "Add exclusion", class = "btn-primary"),
      actionButton(ns("remove_selected"), "Remove selected", class = "btn-warning", style = "margin-top:10px;"),
      hr(),
      downloadButton(ns("download_audit"), "Download audit log"),
      downloadButton(ns("download_doc"), "Download documentation", style = "margin-top:10px;")
    ),
    tagList(
      fluidRow(
        column(6,
               card(
                 card_header("Current EY exclusions"),
                 DTOutput(ns("exclusion_table"))
               )
        ),
        column(6,
               card(
                 card_header("Audit trail"),
                 DTOutput(ns("audit_table"))
               )
        )
      ),
      card(
        card_header("Compliance documentation"),
        textAreaInput(ns("documentation"), label = NULL, rows = 12,
                      placeholder = "Capture policy references, meeting notes, and exceptions."),
        actionButton(ns("save_doc"), "Save note", class = "btn-success", style = "margin-top:10px;")
      )
    )
  )
}

compliance_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "exclusion_symbol", choices = symbol_choices, server = TRUE)

    state <- reactiveValues(
      exclusions = data.frame(Ticker = character(), Reason = character(), Notes = character(), Added = character(), stringsAsFactors = FALSE),
      audit = data.frame(Time = character(), Action = character(), Details = character(), stringsAsFactors = FALSE),
      documentation = ""
    )

    append_audit <- function(action, details) {
      entry <- data.frame(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Action = action, Details = details, stringsAsFactors = FALSE)
      state$audit <- rbind(entry, state$audit)
    }

    observeEvent(input$add_exclusion, {
      sym <- resolve_to_symbol(input$exclusion_symbol)
      validate(need(!is.na(sym), "Invalid symbol."))
      if (sym %in% state$exclusions$Ticker) {
        showNotification("Ticker already on the exclusion list.", type = "warning")
        return()
      }
      notes <- trimws(input$exclusion_notes)
      state$exclusions <- rbind(state$exclusions,
                                data.frame(Ticker = sym, Reason = input$exclusion_reason, Notes = notes, Added = as.character(Sys.Date()), stringsAsFactors = FALSE))
      append_audit("Add exclusion", sprintf("%s - %s", sym, input$exclusion_reason))
      showNotification(sprintf("%s added to exclusions", sym), type = "message")
      updateTextAreaInput(session, "exclusion_notes", value = "")
    })

    observeEvent(input$remove_selected, {
      sel <- input$exclusion_table_rows_selected
      if (!length(sel)) {
        showNotification("Select at least one row to remove.", type = "warning")
        return()
      }
      removed <- state$exclusions$Ticker[sel]
      state$exclusions <- state$exclusions[-sel, , drop = FALSE]
      append_audit("Remove exclusion", paste(removed, collapse = ", "))
      showNotification(sprintf("Removed: %s", paste(removed, collapse = ", ")), type = "message")
    })

    observeEvent(input$save_doc, {
      state$documentation <- input$documentation
      append_audit("Update documentation", sprintf("%d characters", nchar(input$documentation)))
      showNotification("Documentation saved", type = "message")
    })

    output$exclusion_table <- renderDT({
      datatable(state$exclusions, selection = "single", options = list(pageLength = 5))
    })

    output$audit_table <- renderDT({
      datatable(state$audit, options = list(pageLength = 8, dom = 't'))
    })

    output$download_audit <- downloadHandler(
      filename = function() sprintf("audit_log_%s.csv", Sys.Date()),
      content = function(file) {
        write.csv(state$audit, file, row.names = FALSE)
      }
    )

    output$download_doc <- downloadHandler(
      filename = function() sprintf("compliance_notes_%s.txt", Sys.Date()),
      content = function(file) {
        cat(state$documentation, file = file)
      }
    )
  })
}

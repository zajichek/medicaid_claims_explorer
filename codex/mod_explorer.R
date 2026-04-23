mod_explorer_ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    sidebar = sidebar(
      width = 340,
      selectInput(ns("provider_view"), "Provider view", choices = c("Billing provider" = "billing", "Servicing provider" = "servicing")),
      textInput(ns("month_start"), "Start month", placeholder = "YYYY-MM"),
      textInput(ns("month_end"), "End month", placeholder = "YYYY-MM"),
      selectizeInput(
        ns("hcpcs_code"),
        "HCPCS code",
        choices = NULL,
        options = list(placeholder = "Any HCPCS", create = TRUE)
      ),
      textInput(ns("billing_npi"), "Billing provider NPI"),
      textInput(ns("servicing_npi"), "Servicing provider NPI"),
      numericInput(ns("min_total_paid"), "Minimum summarized paid", value = 0, min = 0, step = 1000),
      numericInput(ns("min_total_patients"), "Minimum summarized patients", value = 0, min = 0, step = 10),
      numericInput(ns("row_limit"), "Maximum rows", value = 100, min = 10, max = 1000, step = 10),
      actionButton(ns("apply"), "Apply", class = "btn-primary")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Filtered paid"),
        h4(textOutput(ns("filtered_paid"), inline = TRUE))
      ),
      card(
        card_header("Filtered patients"),
        h4(textOutput(ns("filtered_patients"), inline = TRUE))
      ),
      card(
        card_header("Paid per patient"),
        h4(textOutput(ns("filtered_paid_per_patient"), inline = TRUE))
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Provider summary"),
        DTOutput(ns("provider_table"))
      ),
      card(
        full_screen = TRUE,
        card_header("Selected provider trend"),
        plotOutput(ns("provider_trend"), height = 330)
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Provider-HCPCS summary"),
      DTOutput(ns("provider_hcpcs_table"))
    )
  )
}

mod_explorer_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    filters <- eventReactive(input$apply, {
      list(
        provider_view = input$provider_view,
        month_start = input$month_start,
        month_end = input$month_end,
        hcpcs_code = input$hcpcs_code,
        billing_npi = input$billing_npi,
        servicing_npi = input$servicing_npi,
        min_total_paid = input$min_total_paid,
        min_total_patients = input$min_total_patients,
        row_limit = bounded_limit(input$row_limit)
      )
    }, ignoreNULL = TRUE)

    filtered_tbl <- reactive({
      f <- filters()
      apply_medicaid_filters(
        dataset(),
        month_start = f$month_start,
        month_end = f$month_end,
        hcpcs_code = f$hcpcs_code,
        billing_npi = f$billing_npi,
        servicing_npi = f$servicing_npi
      )
    })

    filtered_metrics <- reactive({
      overview_metrics(filtered_tbl())
    })

    provider_data <- reactive({
      f <- filters()
      provider_summary(
        dataset(),
        provider_view = f$provider_view,
        n = f$row_limit,
        month_start = f$month_start,
        month_end = f$month_end,
        hcpcs_code = f$hcpcs_code,
        billing_npi = f$billing_npi,
        servicing_npi = f$servicing_npi,
        min_total_paid = f$min_total_paid,
        min_total_patients = f$min_total_patients
      )
    })

    selected_provider <- reactive({
      selected <- input$provider_table_rows_selected
      data <- provider_data()

      if (is.null(selected) || length(selected) == 0 || nrow(data) == 0) {
        return("")
      }

      data$provider_npi[[selected[[1]]]]
    })

    provider_hcpcs_data <- reactive({
      f <- filters()
      provider_npi <- selected_provider()
      req(nzchar(provider_npi))

      provider_hcpcs_summary(
        dataset(),
        provider_npi = provider_npi,
        provider_view = f$provider_view,
        n = f$row_limit,
        month_start = f$month_start,
        month_end = f$month_end,
        hcpcs_code = f$hcpcs_code,
        billing_npi = f$billing_npi,
        servicing_npi = f$servicing_npi,
        min_total_paid = f$min_total_paid,
        min_total_patients = f$min_total_patients
      )
    })

    provider_trend_data <- reactive({
      f <- filters()
      provider_npi <- selected_provider()
      req(nzchar(provider_npi))

      provider_monthly_trend(
        dataset(),
        provider_npi = provider_npi,
        provider_view = f$provider_view,
        month_start = f$month_start,
        month_end = f$month_end,
        hcpcs_code = f$hcpcs_code,
        billing_npi = f$billing_npi,
        servicing_npi = f$servicing_npi
      )
    })

    output$filtered_paid <- renderText({
      req(input$apply > 0)
      format_dollars(filtered_metrics()$total_paid[[1]])
    })

    output$filtered_patients <- renderText({
      req(input$apply > 0)
      format_count(filtered_metrics()$total_patients[[1]])
    })

    output$filtered_paid_per_patient <- renderText({
      req(input$apply > 0)
      format_dollars(filtered_metrics()$paid_per_patient[[1]])
    })

    output$provider_table <- renderDT({
      req(input$apply > 0)
      datatable(
        format_query_table(provider_data()),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    output$provider_hcpcs_table <- renderDT({
      validate(need(input$apply > 0, "Click Apply to run provider queries."))
      validate(need(nzchar(selected_provider()), "Select a provider row to load its HCPCS summary."))

      datatable(
        format_query_table(provider_hcpcs_data()),
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    output$provider_trend <- renderPlot({
      validate(need(input$apply > 0, "Click Apply to run provider queries."))
      data <- provider_trend_data()

      validate(
        need(nzchar(selected_provider()), "Select a provider row to show its monthly trend."),
        need(nrow(data) > 0, "No monthly trend is available for the selected provider.")
      )

      x <- seq_len(nrow(data))

      plot(
        x,
        data$total_paid,
        type = "l",
        lwd = 2,
        col = "#2c7fb8",
        xlab = "Claim month",
        ylab = "Total paid",
        axes = FALSE
      )
      axis(1, at = x, labels = format_month_label(data$claim_month), las = 2, cex.axis = 0.8)
      axis(2, labels = format_dollars(axTicks(2)), at = axTicks(2), las = 1)
      box()
      grid(col = "#d9d9d9")
      lines(x, data$total_paid, lwd = 2, col = "#2c7fb8")
    })
  })
}

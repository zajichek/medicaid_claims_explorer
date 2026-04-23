mod_overview_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    card(
      card_body(
        actionButton(ns("load_overview"), "Load Overview", class = "btn-primary")
      )
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      card(
        card_header("Total paid"),
        h3(textOutput(ns("total_paid"), inline = TRUE))
      ),
      card(
        card_header("Rows"),
        h3(textOutput(ns("row_count"), inline = TRUE))
      ),
      card(
        card_header("Billing providers"),
        h3(textOutput(ns("billing_provider_count"), inline = TRUE))
      ),
      card(
        card_header("HCPCS codes"),
        h3(textOutput(ns("hcpcs_count"), inline = TRUE))
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Monthly paid trend"),
        plotOutput(ns("monthly_trend"), height = 360)
      ),
      card(
        full_screen = TRUE,
        card_header("Top HCPCS by paid amount"),
        DTOutput(ns("top_hcpcs"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Top billing providers by paid amount"),
      DTOutput(ns("top_providers"))
    )
  )
}

mod_overview_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    metrics <- eventReactive(input$load_overview, {
      overview_metrics(dataset())
    }, ignoreNULL = TRUE)

    trend <- eventReactive(input$load_overview, {
      monthly_trend(dataset())
    }, ignoreNULL = TRUE)

    hcpcs_data <- eventReactive(input$load_overview, {
      top_hcpcs(dataset(), n = 25)
    }, ignoreNULL = TRUE)

    provider_data <- eventReactive(input$load_overview, {
      top_billing_providers(dataset(), n = 25)
    }, ignoreNULL = TRUE)

    output$total_paid <- renderText({
      req(input$load_overview > 0)
      format_dollars(metrics()$total_paid[[1]])
    })

    output$row_count <- renderText({
      req(input$load_overview > 0)
      format_count(metrics()$row_count[[1]])
    })

    output$billing_provider_count <- renderText({
      req(input$load_overview > 0)
      format_count(metrics()$billing_provider_count[[1]])
    })

    output$hcpcs_count <- renderText({
      req(input$load_overview > 0)
      format_count(metrics()$hcpcs_count[[1]])
    })

    output$monthly_trend <- renderPlot({
      validate(need(input$load_overview > 0, "Click Load Overview to run the overview queries."))
      data <- trend()
      req(nrow(data) > 0)

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
      axis(1, at = x, labels = format_month_label(data$claim_month), las = 2, cex.axis = 0.7)
      axis(2, labels = format_dollars(axTicks(2)), at = axTicks(2), las = 1)
      box()
      grid(col = "#d9d9d9")
      lines(x, data$total_paid, lwd = 2, col = "#2c7fb8")
    })

    output$top_hcpcs <- renderDT({
      req(input$load_overview > 0)
      datatable(
        format_query_table(hcpcs_data()),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$top_providers <- renderDT({
      req(input$load_overview > 0)
      datatable(
        format_query_table(provider_data()),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  })
}

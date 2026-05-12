mod_map_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Map readiness"),
        actionButton(ns("load_map_summary"), "Check Source", class = "btn-primary"),
        p("Geographic enrichment can be added later by joining provider or region coordinates to the lazy DuckDB source."),
        verbatimTextOutput(ns("map_summary"))
      ),
      card(
        full_screen = TRUE,
        card_header("Provider geography placeholder"),
        leafletOutput(ns("map"), height = 560)
      )
    )
  )
}

mod_map_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    metrics <- eventReactive(input$load_map_summary, {
      overview_metrics(dataset())
    }, ignoreNULL = TRUE)

    output$map_summary <- renderPrint({
      validate(need(input$load_map_summary > 0, "Click Check Source to run the source summary."))
      data <- metrics()
      cat("Billing providers ready for enrichment: ", format_count(data$billing_provider_count[[1]]), "\n", sep = "")
      cat("Servicing providers ready for enrichment: ", format_count(data$servicing_provider_count[[1]]), "\n", sep = "")
      cat("Primary join candidates: BILLING_PROVIDER_NPI_NUM, SERVICING_PROVIDER_NPI_NUM\n")
    })

    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) |>
        addControl(
          html = "Coordinate enrichment pending",
          position = "topright"
        )
    })
  })
}

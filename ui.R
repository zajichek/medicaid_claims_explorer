# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer
# Description: Creates the UI objects

ui <-
  page_sidebar(
    theme = bs_theme(bootswatch = "spacelab"),
    title = tags$h2(
      class = "bslib-page-title",
      style = "color:white;",
      "Medicaid Spending",
      tags$span(
        class = "h6",
        style = "color:white",
        "Wisconsin 2018-2024"
      )
    ),
    window_title = "Medicaid Spending Wisconsin",

    # Sidebar holds configuration
    sidebar = sidebar(
      open = TRUE,
      width = 350,

      h2("Filters", style = "text-align:center"),

      # Provider Information (Type I NPI)
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Individual Providers",
          icon = icon("user-doctor"),

          # Simultaneously filtering on columns
          select_group_ui(
            id = "providers",
            params = list(
              list(inputId = "NPI", label = "NPI #"),
              list(inputId = "LastName", label = "Last Name"),
              list(inputId = "FirstName", label = "First Name"),
              list(inputId = "Credentials", label = "Credentials"),
              list(inputId = "TaxonomyCode", label = "Taxonomy"),
              list(inputId = "City", label = "City"),
              list(inputId = "Zip", label = "Zip"),
              list(inputId = "Sex", label = "Sex")
            ),
            inline = FALSE
          )
        )
      ),

      # Provider Information (Type II NPI)
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Organizations",
          icon = icon("user-doctor"),

          # Simultaneously filtering on columns
          select_group_ui(
            id = "organizations",
            params = list(
              list(inputId = "NPI", label = "NPI #"),
              list(inputId = "Name", label = "Name"),
              list(inputId = "TaxonomyCode", label = "Taxonomy"),
              list(inputId = "Subpart", label = "Subpart of org?"),
              list(inputId = "City", label = "City"),
              list(inputId = "Zip", label = "Zip")
            ),
            inline = FALSE
          )
        )
      ),

      # Dataset sources
      HTML("<br><br>"),
      h3("Data Sources"),
      tags$a(
        "Medicaid Spending Data",
        href = "https://opendata.hhs.gov/datasets/medicaid-provider-spending/"
      ),
      tags$a(
        "NPI Data",
        href = "https://download.cms.gov/nppes/NPI_Files.html"
      ),
      tags$a(
        "HCPCS Code Descriptions",
        href = "https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files/rvu24a"
      ),
      tags$a(
        "HCPCS BETOS Categories",
        href = "https://data.cms.gov/provider-summary-by-type-of-service/provider-service-classifications/restructured-betos-classification-system/data"
      )
    ),

    ### Main output

    # Big content columns
    layout_columns(
      col_widths = c(8, 4),

      # A column that takes up the first fraction of the page
      layout_column_wrap(
        width = 1,
        heights_equal = "row",

        # Map of state
        card(
          card_header(
            div(
              icon("globe"),
              "Utilization Map"
            )
          ),

          # The map object
          leafletOutput(outputId = "utilization_map"),

          full_screen = TRUE
        ),

        # KPI cards + graph across the top
        layout_column_wrap(
          width = 1 / 2,

          #
          highchartOutput(outputId = "scatterplot1"),

          #
          highchartOutput(outputId = "scatterplot2")
        )
      ),

      # Column containing chat pane
      layout_column_wrap(
        width = 1,
        heights_equal = "row",

        # Total Spending
        value_box(
          title = "Total Spending",
          value = textOutput(outputId = "total_spending"),
          showcase = icon("dollar-sign", class = "fa-3x"),
          max_height = "200px",
          full_screen = TRUE
        ),

        # Total claims
        value_box(
          title = "Total Claims",
          value = htmlOutput(outputId = "total_claims"),
          showcase = icon("scale-unbalanced", class = "fa-3x"),
          theme = "danger",
          max_height = "200px",
          full_screen = TRUE
        ),

        # Make a chat UI to interact with
        card(
          card_header(
            div(
              icon("table"),
              "Data View"
            )
          ),

          # Table to showing current selection
          DT::dataTableOutput(outputId = "provider_table"),
          full_screen = TRUE
        )
      )
    )
  )

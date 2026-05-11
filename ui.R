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

      # Claims date range
      sliderTextInput(
        inputId = "month_range",
        label = "Payment Month(s)",
        choices = format(sort(month_map$ClaimMonthDate), "%b %Y"),
        selected = format(
          c(
            min(month_map$ClaimMonthDate),
            max(month_map$ClaimMonthDate)
          ),
          "%b %Y"
        )
      ),

      # HCPCS Codes
      accordion(
        open = FALSE,
        accordion_panel(
          title = "HCPCS Codes",
          icon = icon("list"),

          # Simultaneously filtering on columns
          select_group_ui(
            id = "codes",
            params = list(
              list(inputId = "Type", label = "Code Type"),
              list(inputId = "Category", label = "Category"),
              list(inputId = "Subcategory", label = "Subcategory"),
              list(inputId = "Family", label = "Family"),
              list(
                inputId = "MajorProcedureIndicator",
                label = "Major Procedure?"
              ),
              list(inputId = "CodeDescription", label = "Code")
            ),
            inline = FALSE,
            vs_args = list(
              search = TRUE,
              disableSelectAll = FALSE
            )
          )
        )
      ),

      # Provider Information (Type I NPI)
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Individual Providers",
          icon = icon("user-doctor"),

          # Search as service or billing provider
          checkboxGroupInput(
            inputId = "individual_provider_roles",
            label = "Provider Role",
            choices = c("Billing", "Servicing"),
            selected = c("Billing", "Servicing"),
            inline = TRUE
          ),

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
            inline = FALSE,
            vs_args = list(
              search = TRUE,
              disableSelectAll = FALSE
            )
          )
        )
      ),

      # Provider Information (Type II NPI)
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Organizations",
          icon = icon("hospital"),

          # Search as service or billing provider
          checkboxGroupInput(
            inputId = "organization_provider_roles",
            label = "Provider Role",
            choices = c("Billing", "Servicing"),
            selected = c("Billing", "Servicing"),
            inline = TRUE
          ),

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
            inline = FALSE,
            vs_args = list(
              search = TRUE,
              disableSelectAll = FALSE
            )
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

    value_box(
      title = "Claim Rows Selected",
      value = textOutput(outputId = "claim_row_count"),
      showcase = icon("hospital", class = "fa-3x"),
      max_height = "200px",
      full_screen = TRUE
    ),

    card(
      card_header(
        div(
          icon("table"),
          "Data Preview"
        )
      ),

      # Table to showing current selection
      DT::dataTableOutput(outputId = "claims_table"),
      full_screen = TRUE
    )
  )

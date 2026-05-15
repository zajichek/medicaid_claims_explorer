# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer
# Description: Creates the UI objects

ui <-
  page_sidebar(
    theme = bs_theme(bootswatch = "spacelab"),

    title = div(
      class = "d-flex flex-column",

      div(
        class = "d-flex align-items-center gap-2",

        bs_icon(
          "activity",
          class = "text-info",
          style = "font-size: 1.8rem;"
        ),

        tags$span(
          style = "
        color: white;
        font-size: 1.9rem;
        font-weight: 700;
        letter-spacing: 0.3px;
      ",
          "Wisconsin Medicaid Provider Explorer"
        )
      ),

      tags$div(
        style = "
      color: rgba(255,255,255,0.82);
      font-size: 0.95rem;
      margin-top: 0.15rem;
      margin-left: 2.2rem;
      line-height: 1.2;
    ",
        "Outpatient & Professional Claims • 2018–2024"
      )
    ),

    window_title = "Wisconsin Medicaid Provider Explorer",

    tags$style(HTML(
      "
  .bslib-card {
    margin-bottom: 0.75rem;
  }
  .value-box {
    min-height: 7rem;
  }
"
    )),

    # Sidebar holds configuration
    sidebar = sidebar(
      open = FALSE,
      width = 350,

      h2("Global Filters", style = "text-align:center"),
      div(
        class = "small text-muted text-center mb-3",
        "These filters update every tab in the app."
      ),

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

      # Accordion to select providers
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Providers",
          icon = icon("user-doctor"),

          # Provider Information (Type I NPI)
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Individuals (Type 1)",
              icon = icon("person"),

              # Search as service or billing provider
              checkboxGroupInput(
                inputId = "individual_provider_roles",
                label = "Match selected providers as",
                choices = c("Billing", "Servicing"),
                selected = c("Billing", "Servicing"),
                inline = TRUE
              ),
              div(
                class = "small text-muted mb-2",
                "Choose whether selected NPIs should match billing provider, servicing provider, or both."
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

          br(),

          # Provider Information (Type II NPI)
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Organizations (Type 2)",
              icon = icon("hospital"),

              # Search as service or billing provider
              checkboxGroupInput(
                inputId = "organization_provider_roles",
                label = "Match selected providers as",
                choices = c("Billing", "Servicing"),
                selected = c("Billing", "Servicing"),
                inline = TRUE
              ),
              div(
                class = "small text-muted mb-2",
                "Choose whether selected NPIs should match billing provider, servicing provider, or both."
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
          )
        )
      ),

      # HCPCS Codes
      accordion(
        open = FALSE,
        accordion_panel(
          title = "HCPCS Codes",
          icon = icon("list"),

          # Simultaneously filtering on columns
          div(
            class = "small text-muted mb-2",
            "HCPCS includes CPT procedure codes and other service, supply, drug, and transportation codes."
          ),
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
      )
    ),

    ## Main output pages
    navset_tab(
      # Home page (overview)
      nav_panel(
        title = "Home",

        div(
          class = "small text-muted mb-2",
          icon("circle-info"),
          HTML(
            "<strong>Current selection:</strong> Metrics, charts, and map reflect the global filters in the sidebar."
          )
        ),

        # KPI metrics
        layout_column_wrap(
          width = 1 / 3,

          # Total paid
          value_box(
            title = "Total Spend",
            value = htmlOutput(outputId = "total_spend"),
            showcase = icon("dollar", class = "fa-3x"),
            theme = "teal",
            max_height = "200px",
            full_screen = TRUE
          ),

          # Provder count (billing)
          value_box(
            title = "Billing Providers",
            value = htmlOutput(outputId = "billing_provider_count"),
            showcase = icon("user-doctor", class = "fa-3x"),
            theme = "warning",
            max_height = "200px",
            full_screen = TRUE
          ),

          # Total claim lines
          value_box(
            title = "Claim Lines",
            value = htmlOutput(outputId = "claim_lines"),
            showcase = icon("list", class = "fa-3x"),
            theme = "primary",
            max_height = "200px",
            full_screen = TRUE
          )
        ),

        # Graphs + map
        layout_column_wrap(
          width = 1 / 2,

          # Plots
          layout_column_wrap(
            width = 1,

            # Spend over time
            card(
              card_header(
                div(
                  icon("clock"),
                  "Monthly Spend"
                )
              ),
              highchartOutput(outputId = "spend_over_time") |>
                withSpinner(
                  type = 4,
                  color = "#2C3E50",
                  size = 1
                ),
              div(
                class = "small text-muted mt-2",
                "Monthly totals are based on filtered claim-line payments by payment month."
              ),
              full_screen = TRUE
            )
          ),

          # WI map
          card(
            card_header(
              div(
                icon("globe"),
                "Spend By Billing Provider Location"
              )
            ),

            div(
              class = "small text-muted",
              style = "
    padding: 0.5rem 1rem 0rem 1rem;
    line-height: 1.3;
  ",
              icon("circle-info"),
              HTML(
                "<strong>Map Tip:</strong> Click clusters to zoom into provider locations. Marker color and size reflect total paid amount."
              )
            ),

            leafletOutput(outputId = "county_map") |>
              withSpinner(
                type = 4,
                color = "#2C3E50",
                size = 1
              ),

            full_screen = TRUE
          )
        )
      ),

      # Provider analysis
      nav_panel(
        title = "Provider Analysis",

        # Columns for controls/section metrics
        layout_columns(
          col_widths = c(4, 8),

          # Controls for this provider tab only
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Provider Analysis Controls",
              icon = icon("sliders"),

              div(
                class = "small text-muted mb-2",
                "These controls change this tab only; global filters remain in the sidebar."
              ),

              ## Page-specific selectors

              # Role to analyze
              radioButtons(
                inputId = "pa_provider_role",
                label = "Provider role",
                choices = c(
                  "Billing Provider" = "billing",
                  "Servicing Provider" = "servicing"
                ),
                selected = "billing",
                inline = TRUE
              ),

              # Metric to analyze
              selectInput(
                inputId = "pa_metric",
                label = "Metric",
                choices = c(
                  "Total Paid" = "PaidAmount",
                  "Claim Lines" = "ClaimLines",
                  "Patients" = "Patients",
                  "Paid per Claim Line" = "PaidPerClaimLine"
                ),
                selected = "PaidAmount"
              ),

              # Number of providers to assess
              selectInput(
                inputId = "pa_top_n",
                label = "Top providers",
                choices = c(10, 25, 50, 100),
                selected = 25
              )
            )
          ),

          # Metrics for this provider tab only
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Provider Analysis Metrics",
              icon = icon("chart-area"),

              ## Page-specific summary cards scoped to the currently selected global filters and local role.
              layout_columns(
                col_widths = c(6, 6),

                # Organize in a 2 X 2 grid
                layout_column_wrap(
                  width = 1,
                  value_box(
                    title = "Providers",
                    value = textOutput("pa_provider_count"),
                    showcase = icon("user-doctor")
                  ),
                  value_box(
                    title = "Total Paid",
                    value = textOutput("pa_total_paid"),
                    showcase = icon("dollar-sign"),
                    theme = "teal"
                  )
                ),
                layout_column_wrap(
                  width = 1,
                  value_box(
                    title = "Claim Lines",
                    value = textOutput("pa_claim_lines"),
                    showcase = icon("list"),
                    theme = "primary"
                  ),
                  value_box(
                    title = "Median Paid / Line",
                    value = textOutput("pa_median_paid_per_line"),
                    showcase = icon("chart-line"),
                    theme = "warning"
                  )
                )
              )
            )
          )
        ),

        # Provider result output panels
        layout_columns(
          col_widths = c(7, 5),

          # Scatterplot/bubble chart
          card(
            card_header(
              div(
                icon("chart-line"),
                "Provider Volume vs. Spend"
              )
            ),
            highchartOutput(outputId = "pa_provider_scatter") |>
              withSpinner(
                type = 4,
                color = "#2C3E50",
                size = 1
              ),
            full_screen = TRUE
          ),

          # Provider rankings based on selected metric
          card(
            card_header(
              div(
                icon("ranking-star"),
                "Top Providers"
              )
            ),
            highchartOutput(outputId = "pa_top_provider_bar") |>
              withSpinner(
                type = 4,
                color = "#2C3E50",
                size = 1
              ),
            full_screen = TRUE
          )
        ),

        # Provider level summary table
        card(
          card_header(
            div(
              icon("table"),
              "Provider Summary"
            )
          ),
          div(
            class = "small text-muted mb-2",
            "Provider-level summary for the selected role and metric."
          ),
          DT::dataTableOutput(outputId = "pa_provider_table"),
          full_screen = TRUE
        )
      ),

      # HCPCS Code analysis
      nav_panel(
        title = "Code Analysis",

        # Match Provider Analysis: controls and metrics share the first row.
        layout_columns(
          col_widths = c(4, 8),

          # Controls for this HCPCS tab only.
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Code Analysis Controls",
              icon = icon("sliders"),

              div(
                class = "small text-muted mb-2",
                "These controls change this tab only; global filters remain in the sidebar."
              ),

              ## Page-specific selectors

              # Grouping level for charts and table.
              selectInput(
                inputId = "ca_grouping",
                label = "Group codes by",
                choices = c(
                  "Category" = "HCPCSCategory",
                  "Individual Code" = "HCPCSCode",
                  "Code Type" = "HCPCSType",
                  "Subcategory" = "HCPCSSubcategory",
                  "Family" = "HCPCSFamily",
                  "Major Procedure?" = "HCPCSMajorProcedureIndicator"
                ),
                selected = "HCPCSCategory"
              ),

              # Metric used for ranking and chart y-axis.
              selectInput(
                inputId = "ca_metric",
                label = "Metric",
                choices = c(
                  "Total Paid" = "PaidAmount",
                  "Claim Lines" = "ClaimLines",
                  "Patients" = "Patients",
                  "Paid per Claim Line" = "PaidPerClaimLine",
                  "Billing Providers" = "BillingProviders",
                  "Servicing Providers" = "ServicingProviders"
                ),
                selected = "PaidAmount"
              ),

              # Number of code groups to show in the bar chart/table.
              selectInput(
                inputId = "ca_top_n",
                label = "Top groups",
                choices = c(10, 25, 50, 100),
                selected = 25
              )
            )
          ),

          # Metrics for this HCPCS tab only.
          # This mirrors Provider Analysis Metrics while using code-specific outputs.
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Code Analysis Metrics",
              icon = icon("chart-area"),

              ## Page-specific summary cards scoped to the currently selected global
              ## filters and local grouping choice.
              layout_columns(
                col_widths = c(4, 4, 4),

                # Organize compact metric cards in two rows.
                layout_column_wrap(
                  width = 1,
                  value_box(
                    title = "Codes",
                    value = textOutput("ca_code_count"),
                    showcase = icon("barcode")
                  ),
                  value_box(
                    title = "Groups",
                    value = textOutput("ca_group_count"),
                    showcase = icon("layer-group")
                  )
                ),
                layout_column_wrap(
                  width = 1,
                  value_box(
                    title = "Total Paid",
                    value = textOutput("ca_total_paid"),
                    showcase = icon("dollar-sign"),
                    theme = "teal"
                  ),
                  value_box(
                    title = "Claim Lines",
                    value = textOutput("ca_claim_lines"),
                    showcase = icon("list"),
                    theme = "primary"
                  )
                ),
                layout_column_wrap(
                  width = 1,
                  value_box(
                    title = "Median Paid / Line",
                    value = textOutput("ca_median_paid_per_line"),
                    showcase = icon("chart-line"),
                    theme = "warning"
                  )
                )
              )
            )
          )
        ),

        # Match Provider Analysis: wide pattern chart + narrower ranking chart.
        layout_columns(
          col_widths = c(7, 5),

          # Bubble/scatter chart for code groups.
          card(
            card_header(
              div(
                icon("chart-line"),
                "Code Volume vs. Spend"
              )
            ),
            highchartOutput(outputId = "ca_code_scatter") |>
              withSpinner(
                type = 4,
                color = "#2C3E50",
                size = 1
              ),
            full_screen = TRUE
          ),

          # Ranking chart based on selected metric/grouping.
          card(
            card_header(
              div(
                icon("ranking-star"),
                "Top Codes / Code Groups"
              )
            ),
            highchartOutput(outputId = "ca_top_code_bar") |>
              withSpinner(
                type = 4,
                color = "#2C3E50",
                size = 1
              ),
            full_screen = TRUE
          )
        ),

        # Match Provider Analysis: full-width table below charts.
        card(
          card_header(
            div(
              icon("table"),
              "Code Summary Table"
            )
          ),
          div(
            class = "small text-muted mb-2",
            "Code or code-group summary based on the selected grouping level."
          ),
          DT::dataTableOutput(outputId = "ca_code_table") |>
            withSpinner(
              type = 4,
              color = "#2C3E50",
              size = 1
            ),
          full_screen = TRUE
        )
      ),

      # Line-item data + download
      nav_panel(
        title = "Data View",

        # Show filtered result summaries
        layout_column_wrap(
          width = 1 / 4,
          value_box(
            title = "Rows",
            value = textOutput("dv_selected_rows"),
            showcase = icon("table")
          ),
          value_box(
            title = "Total Paid",
            value = textOutput("dv_total_paid"),
            showcase = icon("dollar-sign"),
            theme = "teal"
          ),
          value_box(
            title = "Claim Lines",
            value = textOutput("dv_claim_lines"),
            showcase = icon("list"),
            theme = "primary"
          ),
          value_box(
            title = "Providers",
            value = textOutput("dv_provider_count"),
            showcase = icon("user-doctor"),
            theme = "warning"
          )
        ),

        # Show table
        card(
          card_header(
            div(
              icon("table"),
              "Data Preview"
            )
          ),

          # Download data file button
          div(
            class = "d-flex align-items-center gap-2 mb-2",
            downloadButton(
              outputId = "download_claims_preview",
              label = "Download CSV"
            ),
            span(
              class = "small text-muted",
              "Downloads are limited to the first 1,000 filtered records to keep the app responsive."
            )
          ),

          # Table to showing current selection
          div(
            class = "small text-muted mb-2",
            "Preview of enriched filtered claim records. Use column filters to narrow the displayed rows."
          ),
          DT::dataTableOutput(outputId = "claims_table") |>
            withSpinner(
              type = 4,
              color = "#2C3E50",
              size = 1
            ),
          full_screen = TRUE
        )
      ),

      # About the app
      nav_panel(
        title = "About",

        # Organize panes in columns
        layout_columns(
          col_widths = c(6, 6),

          # Data overview
          card(
            full_screen = TRUE,
            card_header("Data overview"),
            accordion(
              open = "What this app analyzes",

              accordion_panel(
                title = "What this app analyzes",
                icon = bs_icon("info-circle", class = "text-primary"),
                p(
                  "This application summarizes Wisconsin Medicaid provider spending using the ",
                  tags$a(
                    "HHS Medicaid Provider Spending dataset.",
                    href = "https://opendata.hhs.gov/datasets/medicaid-provider-spending/"
                  )
                ),
                p(
                  "The data are aggregated from outpatient and professional Medicaid claim lines with populated HCPCS procedure codes."
                ),
                div(
                  class = "alert alert-primary",
                  strong("Unit of analysis: "),
                  "billing provider × servicing provider × HCPCS code × month."
                )
              ),

              accordion_panel(
                title = "How spending totals should be interpreted",
                icon = bs_icon(
                  "bar-chart-line",
                  class = "text-primary"
                ),
                p(
                  "Spending totals in this app represent provider-attributed outpatient and professional Medicaid payments."
                ),
                p(
                  "They should not be interpreted as total Wisconsin Medicaid expenditures. Roughly speaking, the ",
                  tags$a(
                    "annual Medicaid expenditure in Wisconsin",
                    href = "https://usafacts.org/answers/how-much-does-medicaid-cost-in-the-us/state/wisconsin/"
                  ),
                  " has been around $12-13B (while the ",
                  tags$a(
                    "most recent budget",
                    href = "https://www.wpr.org/news/wisconsin-medicaid-expected-spend-213m-over-state-budget#:~:text=It%E2%80%99s%20primarily%20funded%20through%20federal%20dollars%2C%20with%20the%20program%E2%80%99s%20total%20spending%20projected%20to%20be%20%2436.2%20billion%20over%20the%20two%2Dyear%20budget."
                  ),
                  " is closer to $18B). This dataset only accounts for ~$10B in spending over a 7-year period, which amounts to roughly 10% of total Medicaid expenditure over that time period."
                )
              )
            )
          ),

          # Data scope
          card(
            full_screen = TRUE,
            card_header("Scope of the data"),
            accordion(
              open = "What is included",

              accordion_panel(
                title = "What is included",
                icon = bs_icon(
                  "check-circle-fill",
                  class = "text-success"
                ),
                tags$ul(
                  class = "list-unstyled",
                  tags$li(
                    "✅ Outpatient and professional claim-line payments"
                  ),
                  tags$li("✅ HCPCS/CPT-coded services"),
                  tags$li(
                    "✅ Office visits, ED visits, imaging, labs, procedures, drugs, supplies, and transportation"
                  ),
                  tags$li(
                    "✅ Rows with identifiable billing and servicing provider NPIs"
                  ),
                  tags$li(
                    "✅ Provider-level summaries by month, procedure code, claim lines, beneficiaries, and payment amount"
                  )
                ),
                div(
                  class = "alert alert-primary",
                  strong("HCPCS/CPT Codes: "),
                  p(
                    "Individual code labels were obtained from the ",
                    tags$a(
                      "Medicare Physician Fee Schedule",
                      href = "https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files/rvu24a"
                    ),
                    "files, and the code categorizations from the ",
                    tags$a(
                      "BETOS Classification System",
                      href = "https://data.cms.gov/provider-summary-by-type-of-service/provider-service-classifications/restructured-betos-classification-system/data"
                    )
                  )
                )
              ),

              accordion_panel(
                title = "What is not included",
                icon = bs_icon(
                  "exclamation-triangle-fill",
                  class = "text-danger"
                ),
                div(
                  class = "alert alert-danger",
                  strong("Important: "),
                  "This app does not represent total Medicaid program spending."
                ),
                tags$ul(
                  class = "list-unstyled",
                  tags$li(
                    "⚠️ Inpatient hospital facility claims, such as DRG-based admissions"
                  ),
                  tags$li(
                    "⚠️ Most long-term care and institutional care spending"
                  ),
                  tags$li("⚠️ Managed care capitation payments"),
                  tags$li(
                    "⚠️ Supplemental payments and other program-level financial flows"
                  ),
                  tags$li(
                    "⚠️ Claim lines without complete billing and servicing provider attribution"
                  )
                )
              )
            )
          ),

          # Provider definitions
          card(
            full_screen = TRUE,
            card_header("Provider attribution"),
            accordion(
              open = "Billing vs. servicing providers",

              accordion_panel(
                title = "Billing vs. servicing providers",
                icon = bs_icon("people-fill", class = "text-primary"),
                p(
                  strong("Billing provider: "),
                  "the provider or organization responsible for submitting the claim and receiving payment."
                ),
                p(
                  strong("Servicing provider: "),
                  "the individual or organization associated with delivering the service."
                ),
                div(
                  class = "alert alert-secondary",
                  "Both billing and servicing providers may be individual clinicians or healthcare organizations."
                )
              ),

              accordion_panel(
                title = "NPI linkage",
                icon = bs_icon("building", class = "text-primary"),
                p(
                  "Provider information is linked using ",
                  tags$a(
                    "National Provider Identifier (NPI) records.",
                    href = "https://download.cms.gov/nppes/NPI_Files.html"
                  )
                ),
                p(
                  "Provider location is based on NPI registry information and may not perfectly represent where care was delivered."
                )
              )
            )
          ),

          # Limitations
          card(
            full_screen = TRUE,
            card_header("Important limitations"),
            accordion(
              open = "Analytic limitations",

              accordion_panel(
                title = "Analytic limitations",
                icon = bs_icon(
                  "exclamation-circle",
                  class = "text-warning"
                ),
                tags$ul(
                  tags$li(
                    "Beneficiary counts are unique within each provider-code-month cell and should not be summed as unique people across rows."
                  ),
                  tags$li(
                    "Claim-line counts represent service lines, not top-level claims or patient encounters."
                  ),
                  tags$li(
                    "Payment totals are best used for relative comparisons and trend analysis within this outpatient/professional slice of Medicaid."
                  )
                )
              ),

              accordion_panel(
                title = "Recommended interpretation",
                icon = bs_icon("lightbulb", class = "text-warning"),
                p(
                  "This app is best used to explore provider-level and service-level patterns: who is billing, what services are being paid for, how spending changes over time, and which procedures account for the largest payment totals."
                )
              )
            )
          )
        )
      )
    )
  )

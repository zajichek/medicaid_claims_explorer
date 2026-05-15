# ---- Round 1: HCPCS Code Analysis tab suggestions ----

# Short assessment of the current HCPCS Code Analysis tab:
# - UI location:
#     ui.R, inside navset_tab()
# - Current tab definition:
#     nav_panel(title = "HCPCS Code Analysis")
# - Current HCPCS Code Analysis outputs:
#     none identified; the tab appears blank
# - Current selected claims reactive:
#     current_claims()
# - Existing enriched claims helper:
#     enriched_current_claims()
#   This already attaches HCPCS descriptors from hcpcs_lookup.
# - HCPCS lookup object:
#     hcpcs_lookup
# - HCPCS lookup key:
#     Code
# - Available HCPCS grouping/descriptor fields identified:
#     Type, Category, Subcategory, Family, MajorProcedureIndicator,
#     Description, CodeDescription
# - Plotting package already used by the app:
#     highcharter
#   Use highchartOutput/renderHighchart; do not add plotly.
#
# Current claims columns identified:
# - HCPCSCode
# - PaidAmount
# - ClaimLines
# - Patients
# - BillingProvider
# - ServicingProvider

# Recommended first implementation:
# Keep Round 1 small and high-value:
# - local controls for grouping level, metric, and top N
# - summary cards
# - one top-N horizontal bar chart
# - one bubble/scatter chart
# - one summary table
#
# The bar chart must aggregate by the selected grouping level. For example:
# - grouping = HCPCSCategory means one bar per category
# - grouping = HCPCSCode means one bar per individual code
# - grouping = HCPCSSubcategory means one bar per subcategory
#
# Deeper provider-code relationship analysis should wait until Phase 2/3.

# Suggested UI chunk: replace the blank HCPCS Code Analysis nav_panel with this.
# Copy into ui.R where nav_panel(title = "HCPCS Code Analysis") currently appears.
nav_panel(
  title = "HCPCS Code Analysis",

  # Local controls for this tab only. These do not change global filters.
  card(
    card_header(
      div(
        icon("sliders"),
        "HCPCS Analysis Controls"
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
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
      selectInput(
        inputId = "ca_top_n",
        label = "Top groups",
        choices = c(10, 25, 50, 100),
        selected = 25
      )
    )
  ),

  # Summary cards scoped to global filters and local grouping choice.
  layout_column_wrap(
    width = 1 / 5,
    value_box(
      title = "Codes",
      value = textOutput("ca_code_count"),
      showcase = icon("barcode")
    ),
    value_box(
      title = "Groups",
      value = textOutput("ca_group_count"),
      showcase = icon("layer-group")
    ),
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
    ),
    value_box(
      title = "Median Paid / Line",
      value = textOutput("ca_median_paid_per_line"),
      showcase = icon("chart-line"),
      theme = "warning"
    )
  ),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header(
        div(
          icon("chart-scatter"),
          "Code Group Pattern"
        )
      ),
      highchartOutput("ca_code_scatter") |>
        withSpinner(
          type = 4,
          color = "#2C3E50",
          size = 1
        ),
      full_screen = TRUE
    ),
    card(
      card_header(
        div(
          icon("ranking-star"),
          "Top Code Groups"
        )
      ),
      highchartOutput("ca_top_code_bar") |>
        withSpinner(
          type = 4,
          color = "#2C3E50",
          size = 1
        ),
      full_screen = TRUE
    )
  ),

  card(
    card_header(
      div(
        icon("table"),
        "Code Group Summary"
      )
    ),
    DT::dataTableOutput("ca_code_table"),
    full_screen = TRUE
  )
)


# Suggested server helper 1: code_analysis_claims.
# Copy into server.R after enriched_current_claims is defined.
#
# This starts from enriched_current_claims() to avoid repeating descriptor joins.
# Add tab-local code/category filters later if needed; keep Phase 1 scoped to the
# global filters plus local grouping/metric controls.
code_analysis_claims <- reactive({
  enriched_current_claims()
})


# Suggested server helper 2: available grouping choices.
# Copy into server.R after code_analysis_claims.
#
# This validates grouping columns against actual enriched_current_claims() fields.
# It also lets the UI drop choices if a future lookup table changes column names.
available_code_groupings <- reactive({
  candidate_groupings <- c(
    "Individual Code" = "HCPCSCode",
    "Code Type" = "HCPCSType",
    "Category" = "HCPCSCategory",
    "Subcategory" = "HCPCSSubcategory",
    "Family" = "HCPCSFamily",
    "Major Procedure?" = "HCPCSMajorProcedureIndicator"
  )

  candidate_groupings[
    candidate_groupings %in% names(code_analysis_claims())
  ]
})

observeEvent(available_code_groupings(), {
  choices <- available_code_groupings()

  updateSelectInput(
    session = session,
    inputId = "ca_grouping",
    choices = choices,
    selected = if ("HCPCSCategory" %in% choices) {
      "HCPCSCategory"
    } else {
      choices[[1]]
    }
  )
})

selected_code_group_col <- reactive({
  req(input$ca_grouping)
  validate(
    need(
      input$ca_grouping %in% available_code_groupings(),
      "Selected grouping is not available."
    )
  )

  input$ca_grouping
})


# Suggested server helper 3: metric labels.
# Copy near selected_code_group_col.
code_metric_label <- reactive({
  c(
    PaidAmount = "Total Paid",
    ClaimLines = "Claim Lines",
    Patients = "Patients",
    PaidPerClaimLine = "Paid per Claim Line",
    BillingProviders = "Billing Providers",
    ServicingProviders = "Servicing Providers"
  )[[input$ca_metric]]
})


# Suggested server helper 4: code_summary.
# Copy into server.R after selected_code_group_col.
#
# This is the core aggregation for all charts/tables. It aggregates by the
# user-selected grouping level and computes metrics once.
code_summary <- reactive({
  group_col <- selected_code_group_col()

  code_analysis_claims() |>
    mutate(
      CodeGroupValue = coalesce(
        as.character(.data[[group_col]]),
        "[Missing]"
      )
    ) |>
    summarize(
      GroupLabel = if (group_col == "HCPCSCode") {
        first(HCPCSCodeDescription)
      } else {
        first(CodeGroupValue)
      },
      GroupDescription = if (group_col == "HCPCSCode") {
        first(HCPCSDescription)
      } else {
        first(CodeGroupValue)
      },
      DistinctCodes = n_distinct(HCPCSCode),
      BillingProviders = n_distinct(BillingProvider),
      ServicingProviders = n_distinct(ServicingProvider),
      ClaimRows = n(),
      Patients = sum(Patients, na.rm = TRUE),
      ClaimLines = sum(ClaimLines, na.rm = TRUE),
      PaidAmount = sum(PaidAmount, na.rm = TRUE),
      .by = CodeGroupValue
    ) |>
    mutate(
      GroupingColumn = group_col,
      PaidPerClaimLine = if_else(
        ClaimLines > 0,
        PaidAmount / ClaimLines,
        NA_real_
      )
    )
})


# Suggested server helper 5: top N by selected metric.
# Copy after code_summary.
code_top_summary <- reactive({
  req(input$ca_metric, input$ca_top_n)

  code_summary() |>
    mutate(SelectedMetric = .data[[input$ca_metric]]) |>
    arrange(desc(SelectedMetric)) |>
    slice_head(n = as.integer(input$ca_top_n))
})


# Suggested server outputs: summary cards.
# Copy near the HCPCS Code Analysis server outputs.
output$ca_code_count <- renderText({
  format(n_distinct(code_analysis_claims()$HCPCSCode), big.mark = ",")
})

output$ca_group_count <- renderText({
  format(nrow(code_summary()), big.mark = ",")
})

output$ca_total_paid <- renderText({
  scales::dollar(sum(code_summary()$PaidAmount, na.rm = TRUE))
})

output$ca_claim_lines <- renderText({
  format(sum(code_summary()$ClaimLines, na.rm = TRUE), big.mark = ",")
})

output$ca_median_paid_per_line <- renderText({
  scales::dollar(median(code_summary()$PaidPerClaimLine, na.rm = TRUE))
})


# Suggested bubble/scatter chart.
# Copy near the HCPCS Code Analysis outputs.
#
# Chart design:
# - one point per selected code group
# - x-axis: claim lines
# - y-axis: selected metric
# - bubble size: patients when available
# - color grouping can be added later; keep Phase 1 simple
output$ca_code_scatter <- renderHighchart({
  chart_data <-
    code_summary() |>
    mutate(
      SelectedMetric = .data[[input$ca_metric]],
      BubbleSize = pmax(Patients, 1),
      TooltipText = paste0(
        "<b>",
        GroupLabel,
        "</b>",
        "<br>Grouping: ",
        GroupingColumn,
        "<br>Description: ",
        GroupDescription,
        "<br>Total Paid: ",
        scales::dollar(PaidAmount),
        "<br>Claim Lines: ",
        format(ClaimLines, big.mark = ","),
        "<br>Patients: ",
        format(Patients, big.mark = ","),
        "<br>Paid / Claim Line: ",
        scales::dollar(PaidPerClaimLine),
        "<br>Billing Providers: ",
        BillingProviders,
        "<br>Servicing Providers: ",
        ServicingProviders,
        "<br>Distinct Codes: ",
        DistinctCodes
      )
    )

  hchart(
    chart_data,
    "bubble",
    hcaes(
      x = ClaimLines,
      y = SelectedMetric,
      size = BubbleSize,
      name = GroupLabel
    )
  ) |>
    hc_title(text = NULL) |>
    hc_xAxis(
      title = list(text = "Claim Lines")
      # Optional if values are very skewed:
      # type = "logarithmic"
    ) |>
    hc_yAxis(
      title = list(text = code_metric_label())
      # Optional if values are very skewed and always positive:
      # type = "logarithmic"
    ) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormatter = htmlwidgets::JS(
        "function() { return this.options.TooltipText || this.name; }"
      )
    ) |>
    hc_plotOptions(
      bubble = list(
        minSize = 4,
        maxSize = 36,
        marker = list(
          fillOpacity = 0.65,
          lineWidth = 0
        )
      )
    )
})


# Suggested top-N bar chart.
# Copy near the HCPCS Code Analysis outputs.
#
# This respects selected_code_group_col() through code_summary() and
# code_top_summary(). It does not hard-code category-level aggregation.
output$ca_top_code_bar <- renderHighchart({
  chart_data <-
    code_top_summary() |>
    arrange(SelectedMetric) |>
    mutate(
      GroupLabel = forcats::fct_inorder(GroupLabel),
      TooltipText = paste0(
        "<b>",
        GroupLabel,
        "</b>",
        "<br>Grouping: ",
        GroupingColumn,
        "<br>Description: ",
        GroupDescription,
        "<br>",
        code_metric_label(),
        ": ",
        if (input$ca_metric %in% c("PaidAmount", "PaidPerClaimLine")) {
          scales::dollar(SelectedMetric)
        } else {
          format(SelectedMetric, big.mark = ",")
        },
        "<br>Total Paid: ",
        scales::dollar(PaidAmount),
        "<br>Claim Lines: ",
        format(ClaimLines, big.mark = ","),
        "<br>Patients: ",
        format(Patients, big.mark = ","),
        "<br>Billing Providers: ",
        BillingProviders,
        "<br>Servicing Providers: ",
        ServicingProviders,
        "<br>Distinct Codes: ",
        DistinctCodes
      )
    )

  hchart(
    chart_data,
    "bar",
    hcaes(
      x = GroupLabel,
      y = SelectedMetric
    )
  ) |>
    hc_title(text = NULL) |>
    hc_xAxis(title = list(text = NULL)) |>
    hc_yAxis(title = list(text = code_metric_label())) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormatter = htmlwidgets::JS(
        "function() { return this.options.TooltipText || this.category; }"
      )
    )
})


# Suggested code group table.
# Copy near the HCPCS Code Analysis outputs.
output$ca_code_table <- DT::renderDataTable({
  table_data <-
    code_top_summary() |>
    select(
      GroupingColumn,
      CodeGroupValue,
      GroupLabel,
      GroupDescription,
      PaidAmount,
      ClaimLines,
      Patients,
      PaidPerClaimLine,
      BillingProviders,
      ServicingProviders,
      DistinctCodes
    )

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      deferRender = TRUE
    )
  ) |>
    DT::formatCurrency(
      columns = c("PaidAmount", "PaidPerClaimLine"),
      currency = "$",
      digits = 0
    )
})


# Phase 2 code-mix ideas.
# Keep these for later unless the first tab feels too thin.
#
# - top HCPCS categories within selected providers
# - top individual codes within a selected category
# - monthly trend for selected code/group
# - stacked bar of top code groups by provider type
# - provider-code concentration, such as whether spending is dominated by a few
#   providers
# - share of total selected spend by code group

# Phase 3 provider-code relationship ideas.
# Deeper analysis can become dense quickly, so stage it after the basic code
# summaries are validated.
#
# - provider x code group summary table
# - top providers within selected code group
# - code clusters and provider specialization analysis
# - heatmap only when filtered provider/code set is small

# Performance safeguards:
# - Aggregate before plotting.
# - Do not plot raw claim rows.
# - Limit bar charts and tables with top N defaults.
# - Reuse code_summary() across charts and tables.
# - Avoid repeated descriptor joins; use enriched_current_claims() once.
# - Validate grouping columns before using them.
# - If claims later becomes DuckDB-backed, keep the pattern compatible with
#   collect-after-aggregate workflows.

# Staged roadmap:
# Phase 1:
# - local controls
# - code_analysis_claims
# - available_code_groupings / selected_code_group_col
# - code_summary
# - top-N bar chart
# - bubble/scatter chart
# - summary table
#
# Phase 2:
# - code mix by provider role
# - monthly trend by selected top code/group
# - stacked bar by category/subcategory
#
# Phase 3:
# - deeper provider-code relationship analysis
# - code clusters and provider specialization analysis

# Manual validation checklist:
# - HCPCS Code Analysis tab loads.
# - Local controls update charts without affecting global filters.
# - Grouping selector changes bar chart aggregation level.
# - Default grouping uses category or the closest available grouping field.
# - Individual code view shows top individual codes.
# - If global filters restrict to one category, individual code view shows codes
#   within that context.
# - Bubble chart shows one point per code/group.
# - Bar chart ranks selected code groups by selected metric.
# - Tooltips show code descriptors and metric values.
# - Summary table matches chart values.
# - Charts update when global filters change.
# - App remains responsive.
# - No main app files were edited by Codex.

# ---- Round 2: HCPCS Code Analysis layout alignment ----

# Layout-only assessment:
# - The implemented Provider Analysis tab uses a clean, repeated structure:
#     1. layout_columns(col_widths = c(4, 8))
#        - left column: accordion with page-local controls
#        - right column: accordion with page-local metrics
#     2. layout_columns(col_widths = c(7, 5))
#        - left chart card
#        - right ranking card
#     3. full-width summary table card
#
# - The Round 1 HCPCS Code Analysis suggestion used the same content, but its
#   controls were in a card and its metrics were in a full-width value_box row.
#
# - To visually align the HCPCS tab with Provider Analysis, change only the UI
#   layout:
#     - move controls into a left accordion
#     - move metrics into a right accordion
#     - keep the same chart/table output IDs
#     - keep the same server logic, metric definitions, and filtering semantics

# Suggested replacement UI chunk for HCPCS Code Analysis.
# Copy into ui.R where the HCPCS Code Analysis nav_panel currently appears.
#
# Replace either:
#   nav_panel(title = "HCPCS Code Analysis")
#
# or the Round 1 HCPCS Code Analysis nav_panel UI chunk if you already copied it.
#
# This chunk assumes these Round 1 output/control IDs already exist in server.R:
# Controls:
# - ca_grouping
# - ca_metric
# - ca_top_n
#
# Metric outputs:
# - ca_code_count
# - ca_group_count
# - ca_total_paid
# - ca_claim_lines
# - ca_median_paid_per_line
#
# Main outputs:
# - ca_code_scatter
# - ca_top_code_bar
# - ca_code_table
#
# All controls below are local to the HCPCS Code Analysis tab and should not
# affect global sidebar filters.
nav_panel(
  title = "HCPCS Code Analysis",

  # Match Provider Analysis: controls and metrics share the first row.
  layout_columns(
    col_widths = c(4, 8),

    # Controls for this HCPCS tab only.
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Code Analysis Controls",
        icon = icon("sliders"),

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
    DT::dataTableOutput(outputId = "ca_code_table") |>
      withSpinner(
        type = 4,
        color = "#2C3E50",
        size = 1
      ),
    full_screen = TRUE
  )
)

# Round 2 layout validation checklist:
# - HCPCS Code Analysis tab loads.
# - Layout visually matches Provider Analysis.
# - Controls are grouped in a left accordion.
# - Metrics are grouped in a right accordion.
# - Charts appear in the same 7/5 pattern as Provider Analysis.
# - Summary table appears full-width below the charts.
# - Output IDs still match Round 1 server code.
# - Local controls remain scoped to HCPCS Code Analysis.
# - No main app files were edited by Codex.

# ---- Round 1: Provider Analysis tab suggestions ----

# Short assessment of the current Provider Analysis tab:
# - UI location:
#     ui.R, inside navset_tab()
# - Current tab definition:
#     nav_panel(title = "Provider Analysis")
# - Current Provider Analysis outputs:
#     none identified; the tab is an empty page shell
# - Current selected claims reactive:
#     current_claims()
# - Existing enriched claims helper:
#     enriched_current_claims()
#   This already joins billing provider, servicing provider, and HCPCS descriptors.
# - Plotting package already used by the app:
#     highcharter
#   Use highchartOutput/renderHighchart for this tab rather than adding plotly.
#
# Current claims columns identified:
# - BillingProvider
# - ServicingProvider
# - HCPCSCode
# - ClaimMonth
# - Patients
# - ClaimLines
# - PaidAmount
#
# Lookup objects available:
# - providers, keyed by NPI
# - organizations, keyed by NPI
# - hcpcs_lookup, keyed by Code


# Recommended first implementation:
# Keep this tab focused on provider-level exploration inside the global filter
# context. The global sidebar already controls date, provider/org, roles, and
# HCPCS selections, so the tab-local controls should only change analysis view,
# not mutate global filters.
#
# Phase 1 should include:
# - local controls for provider role, metric, and top N
# - summary cards
# - one provider bubble/scatter chart
# - one top provider bar chart
# - one provider summary table
#
# Leave code mix, relationship heatmaps, and network-style views for later.


# Suggested UI chunk: replace the empty Provider Analysis nav_panel with this.
# Copy into ui.R where nav_panel(title = "Provider Analysis") currently appears.
nav_panel(
  title = "Provider Analysis",

  # Local controls for this tab only. These should not affect global filters.
  card(
    card_header(
      div(
        icon("sliders"),
        "Provider Analysis Controls"
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
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
      selectInput(
        inputId = "pa_top_n",
        label = "Top providers",
        choices = c(10, 25, 50, 100),
        selected = 25
      )
    )
  ),

  # Summary cards scoped to the currently selected global filters and local role.
  layout_column_wrap(
    width = 1 / 4,
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
    ),
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
  ),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header(
        div(
          icon("chart-scatter"),
          "Provider Spending Pattern"
        )
      ),
      highchartOutput("pa_provider_scatter") |>
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
          "Top Providers"
        )
      ),
      highchartOutput("pa_top_provider_bar") |>
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
        "Provider Summary"
      )
    ),
    DT::dataTableOutput("pa_provider_table"),
    full_screen = TRUE
  )
)


# Suggested server helper 1: provider_analysis_claims.
# Copy into server.R after enriched_current_claims is defined.
#
# This starts from enriched_current_claims() so it can reuse existing descriptor
# joins. A tab-local HCPCS/category filter can be added later, but keep Phase 1
# simple and let global filters define the claim context.
provider_analysis_claims <- reactive({
  enriched_current_claims()
})


# Suggested server helper 2: provider_summary.
# Copy into server.R after provider_analysis_claims.
#
# This aggregates to one row per provider for either billing or servicing view.
# It does not plot raw claim rows.
provider_summary <- reactive({
  req(input$pa_provider_role)

  if (input$pa_provider_role == "billing") {
    provider_analysis_claims() |>
      summarize(
        ProviderName = first(BillingProviderName),
        ProviderType = first(BillingProviderType),
        City = first(BillingProviderCity),
        State = first(BillingProviderState),
        Zip = first(BillingProviderZip),
        ClaimRows = n(),
        Patients = sum(Patients, na.rm = TRUE),
        ClaimLines = sum(ClaimLines, na.rm = TRUE),
        PaidAmount = sum(PaidAmount, na.rm = TRUE),
        DistinctHCPCS = n_distinct(HCPCSCode),
        DistinctCounterparties = n_distinct(ServicingProvider),
        .by = BillingProvider
      ) |>
      rename(NPI = BillingProvider)
  } else {
    provider_analysis_claims() |>
      summarize(
        ProviderName = first(ServicingProviderName),
        ProviderType = first(ServicingProviderType),
        City = first(ServicingProviderCity),
        State = first(ServicingProviderState),
        Zip = first(ServicingProviderZip),
        ClaimRows = n(),
        Patients = sum(Patients, na.rm = TRUE),
        ClaimLines = sum(ClaimLines, na.rm = TRUE),
        PaidAmount = sum(PaidAmount, na.rm = TRUE),
        DistinctHCPCS = n_distinct(HCPCSCode),
        DistinctCounterparties = n_distinct(BillingProvider),
        .by = ServicingProvider
      ) |>
      rename(NPI = ServicingProvider)
  } |>
    mutate(
      PaidPerClaimLine = if_else(
        ClaimLines > 0,
        PaidAmount / ClaimLines,
        NA_real_
      ),
      ProviderLabel = case_when(
        !is.na(ProviderName) & ProviderName != "" ~ ProviderName,
        TRUE ~ NPI
      )
    )
})


# Suggested server helper 3: selected metric and top provider summary.
# Copy into server.R after provider_summary.
provider_metric_label <- reactive({
  c(
    PaidAmount = "Total Paid",
    ClaimLines = "Claim Lines",
    Patients = "Patients",
    PaidPerClaimLine = "Paid per Claim Line"
  )[[input$pa_metric]]
})

provider_top_summary <- reactive({
  req(input$pa_metric, input$pa_top_n)

  provider_summary() |>
    mutate(SelectedMetric = .data[[input$pa_metric]]) |>
    arrange(desc(SelectedMetric)) |>
    slice_head(n = as.integer(input$pa_top_n))
})


# Suggested server outputs: summary cards.
# Copy near the other Provider Analysis server outputs.
output$pa_provider_count <- renderText({
  format(nrow(provider_summary()), big.mark = ",")
})

output$pa_total_paid <- renderText({
  scales::dollar(sum(provider_summary()$PaidAmount, na.rm = TRUE))
})

output$pa_claim_lines <- renderText({
  format(sum(provider_summary()$ClaimLines, na.rm = TRUE), big.mark = ",")
})

output$pa_median_paid_per_line <- renderText({
  scales::dollar(
    median(provider_summary()$PaidPerClaimLine, na.rm = TRUE)
  )
})


# Suggested bubble/scatter chart.
# Copy near the Provider Analysis outputs.
#
# Chart design:
# - one point per provider
# - x-axis: claim lines
# - y-axis: selected metric
# - bubble size: patients when available
# - hover: descriptors and metrics
# - optional log axis comments included below
output$pa_provider_scatter <- renderHighchart({
  chart_data <-
    provider_summary() |>
    mutate(
      SelectedMetric = .data[[input$pa_metric]],
      BubbleSize = pmax(Patients, 1),
      TooltipText = paste0(
        "<b>", ProviderLabel, "</b>",
        "<br>NPI: ", NPI,
        "<br>Type: ", ProviderType,
        "<br>Location: ", City, ", ", State, " ", Zip,
        "<br>Total Paid: ", scales::dollar(PaidAmount),
        "<br>Claim Lines: ", format(ClaimLines, big.mark = ","),
        "<br>Patients: ", format(Patients, big.mark = ","),
        "<br>Paid / Claim Line: ", scales::dollar(PaidPerClaimLine),
        "<br>Distinct HCPCS: ", DistinctHCPCS
      )
    )

  hchart(
    chart_data,
    "bubble",
    hcaes(
      x = ClaimLines,
      y = SelectedMetric,
      size = BubbleSize,
      name = ProviderLabel
    )
  ) |>
    hc_title(text = NULL) |>
    hc_xAxis(
      title = list(text = "Claim Lines")
      # Optional if values are very skewed:
      # type = "logarithmic"
    ) |>
    hc_yAxis(
      title = list(text = provider_metric_label())
      # Optional if values are very skewed and metric is always positive:
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


# Suggested bar chart.
# Copy near the Provider Analysis outputs.
#
# Horizontal bars keep provider names readable and top N prevents chart overload.
output$pa_top_provider_bar <- renderHighchart({
  chart_data <-
    provider_top_summary() |>
    arrange(SelectedMetric) |>
    mutate(
      ProviderLabel = forcats::fct_inorder(ProviderLabel),
      TooltipText = paste0(
        "<b>", ProviderLabel, "</b>",
        "<br>NPI: ", NPI,
        "<br>Type: ", ProviderType,
        "<br>", provider_metric_label(), ": ",
        if (input$pa_metric %in% c("PaidAmount", "PaidPerClaimLine")) {
          scales::dollar(SelectedMetric)
        } else {
          format(SelectedMetric, big.mark = ",")
        },
        "<br>Total Paid: ", scales::dollar(PaidAmount),
        "<br>Claim Lines: ", format(ClaimLines, big.mark = ","),
        "<br>Patients: ", format(Patients, big.mark = ",")
      )
    )

  hchart(
    chart_data,
    "bar",
    hcaes(
      x = ProviderLabel,
      y = SelectedMetric
    )
  ) |>
    hc_title(text = NULL) |>
    hc_xAxis(title = list(text = NULL)) |>
    hc_yAxis(title = list(text = provider_metric_label())) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormatter = htmlwidgets::JS(
        "function() { return this.options.TooltipText || this.category; }"
      )
    )
})


# Suggested provider summary table.
# Copy near the Provider Analysis outputs.
output$pa_provider_table <- DT::renderDataTable({
  table_data <-
    provider_top_summary() |>
    select(
      NPI,
      ProviderName,
      ProviderType,
      City,
      State,
      Zip,
      PaidAmount,
      ClaimLines,
      Patients,
      PaidPerClaimLine,
      DistinctHCPCS,
      DistinctCounterparties
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


# Phase 2 provider/code mix ideas.
# Keep these as later additions unless the first Provider Analysis tab feels too
# thin after Phase 1.
#
# Provider by BETOS/HCPCS category:
# provider_code_mix <- reactive({
#   provider_analysis_claims() |>
#     summarize(
#       PaidAmount = sum(PaidAmount, na.rm = TRUE),
#       ClaimLines = sum(ClaimLines, na.rm = TRUE),
#       Patients = sum(Patients, na.rm = TRUE),
#       .by = c(BillingProvider, BillingProviderName, HCPCSCategory)
#     )
# })
#
# Possible displays:
# - stacked bar chart for top providers by HCPCSCategory
# - bubble chart colored by dominant HCPCSCategory
# - table of top HCPCS codes/categories for selected providers


# Phase 2/3 billing-servicing relationship ideas.
# Do not implement as the first pass; these can get visually dense quickly.
#
# Billing-to-servicing pair summary:
# provider_relationship_summary <- reactive({
#   enriched_current_claims() |>
#     summarize(
#       PaidAmount = sum(PaidAmount, na.rm = TRUE),
#       ClaimLines = sum(ClaimLines, na.rm = TRUE),
#       Patients = sum(Patients, na.rm = TRUE),
#       HCPCSCount = n_distinct(HCPCSCode),
#       .by = c(
#         BillingProvider,
#         BillingProviderName,
#         ServicingProvider,
#         ServicingProviderName
#       )
#     ) |>
#     arrange(desc(PaidAmount))
# })
#
# Possible displays:
# - top billing-servicing relationships table
# - heatmap when filtered provider set is small
# - network graph only as a later idea; do not add a new graph package now


# Performance safeguards:
# - Aggregate to provider level before plotting.
# - Do not plot raw claim rows.
# - Use top N limits for bars and tables.
# - Avoid huge HTML tooltips.
# - Reuse enriched_current_claims() and provider_summary(); do not recompute
#   descriptor joins separately for each chart.
# - Consider req(nrow(provider_summary()) <= some_limit) before expensive charts
#   if a filtered selection is still very large.
# - Keep network/relationship analysis staged; pairwise billing-servicing
#   summaries can grow quickly.


# Staged roadmap:
# Phase 1:
# - provider_analysis_claims
# - provider_summary
# - bubble/scatter chart
# - top provider bar chart
# - provider summary table
#
# Phase 2:
# - provider/code mix analysis
# - top HCPCS categories per provider
# - provider relationship table
#
# Phase 3:
# - billing-servicing heatmap for small filtered sets
# - network-style analysis only if a suitable package is already adopted later


# Manual validation checklist:
# - Provider Analysis tab loads.
# - Local controls update charts without affecting global filters.
# - Billing vs servicing view changes provider aggregation correctly.
# - Bubble chart shows one point per provider.
# - Bar chart ranks providers by selected metric.
# - Tooltips show provider descriptors and metric values.
# - Provider summary table matches chart values.
# - Charts update when global filters change.
# - App remains responsive.
# - No main app files were edited by Codex.

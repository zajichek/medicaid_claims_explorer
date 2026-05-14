# ---- Round 1: Data View tab suggestions ----

# Short assessment of current Data View structure:
# - Data View UI location:
#     ui.R, inside navset_tab(), in nav_panel(title = "Data View")
# - Current table output id:
#     "claims_table"
# - Current server output/render object:
#     output$claims_table <- DT::renderDataTable({ ... })
# - Current selected claims reactive:
#     current_claims()
# - Current display behavior:
#     current_claims() |> sample_n(min(1000, nrow(current_claims())))
# - Download handler:
#     none found in the inspected app files
# - Lookup/dimension tables available from global.R:
#     providers
#     organizations
#     hcpcs_lookup
# - Existing packages already loaded include:
#     tidyverse, bslib, DT
#
# Useful claims columns identified:
# - BillingProvider
# - ServicingProvider
# - HCPCSCode
# - ClaimMonth
# - Patients
# - ClaimLines
# - PaidAmount
#
# Useful provider lookup columns identified:
# - providers: NPI, LastName, FirstName, Sex, Credentials, City, State, Zip,
#   TaxonomyCode, lon, lat
# - organizations: NPI, Name, City, State, Zip, TaxonomyCode, Subpart, lon, lat
#
# Useful HCPCS lookup columns identified:
# - hcpcs_lookup: Code, Description, Type, Category, Subcategory, Family,
#   MajorProcedureIndicator, CodeDescription

# Recommended Data View design:
# Start with both, but keep Phase 1 small:
# A. Detailed enriched line-item table:
#    Recommended first. It directly improves the current raw claims preview by
#    attaching billing provider, servicing provider, and HCPCS descriptors.
#
# B. Summarized tables:
#    Useful, but better as Phase 2. Add summaries by HCPCS, billing provider,
#    servicing provider, and month only after the enriched table is stable.
#
# Smallest useful first implementation:
# - Add provider_lookup_combined.
# - Add enriched_current_claims.
# - Render a limited enriched table.
# - Add a limited CSV download.
# - Optionally add simple summary cards above the table.

# Suggested UI chunk: summary cards for the Data View tab.
# Copy into ui.R inside nav_panel(title = "Data View"), above the existing table
# card. These are optional but useful, and they reuse server outputs suggested
# below.
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
)


# Suggested UI chunk: download button and note.
# Copy into ui.R inside the Data View table card, above
# DT::dataTableOutput(outputId = "claims_table").
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
)


# Suggested UI chunk: keep or replace the existing detailed table output.
# The current output id can stay the same to minimize server/UI changes.
DT::dataTableOutput(outputId = "claims_table")


# Suggested server chunk 1: unified provider lookup.
# Copy into server.R after current_claims is defined, or earlier if it is used by
# other server reactives. This helper is separate so descriptor joins happen in
# one place rather than inside every output renderer.
provider_lookup_combined <- reactive({
  bind_rows(
    providers |>
      transmute(
        NPI,
        ProviderName = str_squish(paste(FirstName, LastName)),
        ProviderType = "Individual",
        City,
        State,
        Zip,
        TaxonomyCode,
        Credentials,
        Sex,
        Subpart = NA_character_
      ),
    organizations |>
      transmute(
        NPI,
        ProviderName = Name,
        ProviderType = "Organization",
        City,
        State,
        Zip,
        TaxonomyCode,
        Credentials = NA_character_,
        Sex = NA_character_,
        Subpart
      )
  )
})


# Suggested server chunk 2: enriched selected claims.
# Copy into server.R after provider_lookup_combined.
#
# What it does:
# - starts from current_claims()
# - preserves claim-level rows
# - attaches billing provider descriptors
# - attaches servicing provider descriptors
# - attaches HCPCS descriptors
# - uses explicit prefixes to keep billing and servicing fields distinct
enriched_current_claims <- reactive({
  provider_lookup <- provider_lookup_combined()

  billing_provider_lookup <-
    provider_lookup |>
    rename(
      BillingProvider = NPI,
      BillingProviderName = ProviderName,
      BillingProviderType = ProviderType,
      BillingProviderCity = City,
      BillingProviderState = State,
      BillingProviderZip = Zip,
      BillingProviderTaxonomyCode = TaxonomyCode,
      BillingProviderCredentials = Credentials,
      BillingProviderSex = Sex,
      BillingProviderSubpart = Subpart
    )

  servicing_provider_lookup <-
    provider_lookup |>
    rename(
      ServicingProvider = NPI,
      ServicingProviderName = ProviderName,
      ServicingProviderType = ProviderType,
      ServicingProviderCity = City,
      ServicingProviderState = State,
      ServicingProviderZip = Zip,
      ServicingProviderTaxonomyCode = TaxonomyCode,
      ServicingProviderCredentials = Credentials,
      ServicingProviderSex = Sex,
      ServicingProviderSubpart = Subpart
    )

  hcpcs_descriptor_lookup <-
    hcpcs_lookup |>
    transmute(
      HCPCSCode = Code,
      HCPCSDescription = Description,
      HCPCSType = Type,
      HCPCSCategory = Category,
      HCPCSSubcategory = Subcategory,
      HCPCSFamily = Family,
      HCPCSMajorProcedureIndicator = MajorProcedureIndicator,
      HCPCSCodeDescription = CodeDescription
    )

  current_claims() |>
    left_join(
      y = billing_provider_lookup,
      by = "BillingProvider"
    ) |>
    left_join(
      y = servicing_provider_lookup,
      by = "ServicingProvider"
    ) |>
    left_join(
      y = hcpcs_descriptor_lookup,
      by = "HCPCSCode"
    ) |>
    select(
      ClaimMonth,
      BillingProvider,
      BillingProviderName,
      BillingProviderType,
      BillingProviderCity,
      BillingProviderState,
      BillingProviderZip,
      ServicingProvider,
      ServicingProviderName,
      ServicingProviderType,
      ServicingProviderCity,
      ServicingProviderState,
      ServicingProviderZip,
      HCPCSCode,
      HCPCSDescription,
      HCPCSType,
      HCPCSCategory,
      HCPCSSubcategory,
      HCPCSFamily,
      Patients,
      ClaimLines,
      PaidAmount,
      everything()
    )
})


# Suggested server chunk 3: Data View summary outputs.
# Copy into server.R near the Data View output section.
output$dv_selected_rows <- renderText({
  format(nrow(enriched_current_claims()), big.mark = ",")
})

output$dv_total_paid <- renderText({
  scales::dollar(sum(enriched_current_claims()$PaidAmount, na.rm = TRUE))
})

output$dv_claim_lines <- renderText({
  format(
    sum(enriched_current_claims()$ClaimLines, na.rm = TRUE),
    big.mark = ","
  )
})

output$dv_provider_count <- renderText({
  paste0(
    n_distinct(enriched_current_claims()$BillingProvider),
    " billing / ",
    n_distinct(enriched_current_claims()$ServicingProvider),
    " servicing"
  )
})


# Suggested server chunk 4: replace current claims_table renderer.
# Keep output id "claims_table" so the existing UI output can remain unchanged.
#
# Display limit:
# - Shows the first 1,000 enriched rows.
# - This is intentionally a preview, not a full browser-side export.
output$claims_table <- DT::renderDataTable({
  display_data <-
    enriched_current_claims() |>
    slice_head(n = 1000)

  DT::datatable(
    display_data,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      deferRender = TRUE
    )
  ) |>
    DT::formatCurrency(
      columns = "PaidAmount",
      currency = "$",
      digits = 0
    )
})


# Suggested server chunk 5: limited CSV download.
# Copy into server.R near output$claims_table.
#
# This uses enriched_current_claims() so the CSV includes provider and HCPCS
# descriptors. The row limit is intentionally conservative for now; raise it
# later only after checking responsiveness.
output$download_claims_preview <- downloadHandler(
  filename = function() {
    paste0("medicaid_claims_preview_", Sys.Date(), ".csv")
  },
  content = function(file) {
    download_data <-
      enriched_current_claims() |>
      slice_head(n = 1000)

    write.csv(
      download_data,
      file = file,
      row.names = FALSE,
      na = ""
    )
  }
)


# Optional summarized table helpers for Phase 2.
# Do not add all of these in Phase 1 unless the Data View tab needs them right
# away. They are useful candidates for separate cards or tabs later.
spending_by_hcpcs <- reactive({
  enriched_current_claims() |>
    summarize(
      ClaimRows = n(),
      Patients = sum(Patients, na.rm = TRUE),
      ClaimLines = sum(ClaimLines, na.rm = TRUE),
      PaidAmount = sum(PaidAmount, na.rm = TRUE),
      .by = c(HCPCSCode, HCPCSDescription, HCPCSCategory)
    ) |>
    arrange(desc(PaidAmount))
})

spending_by_billing_provider <- reactive({
  enriched_current_claims() |>
    summarize(
      ClaimRows = n(),
      Patients = sum(Patients, na.rm = TRUE),
      ClaimLines = sum(ClaimLines, na.rm = TRUE),
      PaidAmount = sum(PaidAmount, na.rm = TRUE),
      .by = c(BillingProvider, BillingProviderName, BillingProviderType)
    ) |>
    arrange(desc(PaidAmount))
})

spending_by_servicing_provider <- reactive({
  enriched_current_claims() |>
    summarize(
      ClaimRows = n(),
      Patients = sum(Patients, na.rm = TRUE),
      ClaimLines = sum(ClaimLines, na.rm = TRUE),
      PaidAmount = sum(PaidAmount, na.rm = TRUE),
      .by = c(ServicingProvider, ServicingProviderName, ServicingProviderType)
    ) |>
    arrange(desc(PaidAmount))
})

spending_by_month <- reactive({
  enriched_current_claims() |>
    summarize(
      ClaimRows = n(),
      Patients = sum(Patients, na.rm = TRUE),
      ClaimLines = sum(ClaimLines, na.rm = TRUE),
      PaidAmount = sum(PaidAmount, na.rm = TRUE),
      .by = ClaimMonth
    ) |>
    arrange(ClaimMonth)
})

# Performance safeguards:
# - Join descriptors once in enriched_current_claims(), then reuse that reactive.
# - Do not repeat provider/HCPCS joins inside DT renderers or download handlers.
# - Limit displayed rows to 1,000 for now.
# - Limit downloaded rows to 1,000 for now.
# - Add summarized views for large selections rather than trying to display all
#   claim rows in the browser.
# - If claims later becomes DuckDB-backed instead of fully collected in memory,
#   push filtering and summarization into DuckDB before collecting for display.
# - Avoid sample_n() for the main preview if repeatability matters; slice_head()
#   makes table/download contents predictable.

# Staged implementation recommendation:
# Phase 1:
# - Add provider_lookup_combined.
# - Add enriched_current_claims.
# - Replace the raw claims preview with a limited enriched table.
# - Add a limited CSV download.
#
# Phase 2:
# - Add summary cards.
# - Add summarized tables by HCPCS, billing provider, servicing provider, and
#   month.
#
# Phase 3:
# - Add user-controlled download limits or a server-side export strategy.
# - Consider server-side DT processing if the app needs larger interactive tables.

# Manual validation checklist:
# - Data View tab still loads.
# - Table shows enriched provider and HCPCS descriptors.
# - Billing and servicing provider descriptors are clearly distinguished.
# - Table updates when filters change.
# - Display row limit is enforced.
# - Download button creates a CSV.
# - Download includes provider and HCPCS descriptors.
# - Download row limit is enforced.
# - App remains responsive.
# - No main app files were edited by Codex.

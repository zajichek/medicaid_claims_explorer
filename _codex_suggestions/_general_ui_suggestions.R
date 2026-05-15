# ---- Round 1: targeted general UI polish suggestions ----

# Suggestion 1: Rename the sidebar heading from "Filters" to "Global Filters"
# Why:
# - The app now has tab-local controls in Provider Analysis and HCPCS Code
#   Analysis. Calling the sidebar "Global Filters" makes the distinction clearer.
# Where:
# - ui.R
# - sidebar = sidebar(...)
# - UI code
# Code:
# Replace:
#   h2("Filters", style = "text-align:center")
#
# With:
h2("Global Filters", style = "text-align:center")
div(
  class = "small text-muted text-center mb-3",
  "These filters update every tab in the app."
)
# Validation:
# - Sidebar still opens/collapses normally.
# - Tab-local controls remain visually distinct from the global filter pane.

# Suggestion 2: Make the provider role checkbox labels more explanatory
# Why:
# - "Provider Role" is accurate but terse. Users may not immediately know that
#   Billing and Servicing refer to how selected NPIs are matched in claims.
# Where:
# - ui.R
# - Individual and Organization provider accordion panels
# - UI code
# Code:
# Replace label = "Provider Role" in both checkboxGroupInput calls with:
label = "Match selected providers as"
# Optional insertion directly below each checkboxGroupInput:
div(
  class = "small text-muted mb-2",
  "Choose whether selected NPIs should match billing provider, servicing provider, or both."
)
# Validation:
# - The checkbox values and input IDs stay unchanged.
# - Filtering behavior is unchanged.

# Suggestion 3: Add a concise HCPCS/CPT helper note in the HCPCS filter panel
# Why:
# - HCPCS/CPT language is technical. A one-line explanation reduces friction
#   without turning the sidebar into documentation.
# Where:
# - ui.R
# - Sidebar HCPCS Codes accordion_panel, above select_group_ui(id = "codes")
# - UI code
# Code:
div(
  class = "small text-muted mb-2",
  "HCPCS includes CPT procedure codes and other service, supply, drug, and transportation codes."
)
# Validation:
# - The note does not change datamods inputs.
# - The HCPCS panel remains compact.

# Suggestion 4: Add a lightweight Home page context note above KPIs
# Why:
# - The first screen shows metrics immediately, but a short context note helps
#   users understand that all views respond to the global filters.
# Where:
# - ui.R
# - Home nav_panel, just before the KPI layout_column_wrap
# - UI code
# Code:
div(
  class = "small text-muted mb-2",
  icon("circle-info"),
  HTML(
    "<strong>Current selection:</strong> Metrics, charts, and map reflect the global filters in the sidebar."
  )
)
# Validation:
# - The Home page still starts with the main experience, not a landing page.

# Suggestion 5: Rename Home KPI "Providers Paid" to "Billing Providers"
# Why:
# - The current value box title says "Providers Paid" but the primary value is
#   distinct billing providers, with servicing providers as secondary text.
#   A precise label reduces ambiguity.
# Where:
# - ui.R
# - Home value_box with outputId = "billing_provider_count"
# - UI code
# Code:
# Replace:
#   title = "Providers Paid"
#
# With:
title = "Billing Providers"
# Optional server-side text wording improvement:
# In output$billing_provider_count, change secondary span from:
#   " servicing providers"
# To:
#   " distinct servicing providers"
# Validation:
# - No output IDs change.

# Suggestion 6: Add a short caption below Monthly Spend
# Why:
# - The chart is clear, but a caption clarifies that spend is based on filtered
#   claims and payment month.
# Where:
# - ui.R
# - Home Monthly Spend card, below highchartOutput("spend_over_time")
# - UI code
# Code:
div(
  class = "small text-muted mt-2",
  "Monthly totals are based on filtered claim-line payments by payment month."
)
# Validation:
# - Spinner still wraps only the chart output.

# Suggestion 7: Tighten the map instruction text
# Why:
# - The map tip is useful. This version is a little shorter and says clusters
#   represent grouped providers.
# Where:
# - ui.R
# - Home map card instruction div above leafletOutput("county_map")
# - UI code
# Code:
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
)
# Validation:
# - Map output ID and clustering behavior stay unchanged.

# Suggestion 8: Add tab-local control captions for analysis tabs
# Why:
# - Provider Analysis and HCPCS Code Analysis have local controls. A tiny note
#   prevents users from confusing these with global filters.
# Where:
# - ui.R
# - Inside Provider Analysis Controls accordion_panel and Code Analysis Controls
#   accordion_panel, above the first input
# - UI code
# Code:
div(
  class = "small text-muted mb-2",
  "These controls change this tab only; global filters remain in the sidebar."
)
# Validation:
# - Input IDs and server logic remain unchanged.

# Suggestion 9: Use parallel chart titles between Provider and HCPCS tabs
# Why:
# - The analysis tabs already share layout. Matching title patterns makes the app
#   feel more intentional.
# Where:
# - ui.R
# - Provider Analysis and HCPCS Code Analysis chart card headers
# - UI code
# Code:
# Provider scatter card title:
card_header(
  div(
    icon("chart-line"),
    "Provider Volume vs. Spend"
  )
)
#
# HCPCS scatter card title:
card_header(
  div(
    icon("chart-line"),
    "Code Volume vs. Spend"
  )
)
# Validation:
# - No output IDs change.

# Suggestion 10: Add short table captions above DT outputs
# Why:
# - Tables benefit from a plain-language note about limits and sorting/filtering.
#   This is especially helpful for Data View.
# Where:
# - ui.R
# - Inside Data View, Provider Summary, and Code Summary Table cards, above each
#   DT::dataTableOutput(...)
# - UI code
# Code for Data View:
div(
  class = "small text-muted mb-2",
  "Preview of enriched filtered claim records. Use column filters to narrow the displayed rows."
)
#
# Code for Provider Summary:
div(
  class = "small text-muted mb-2",
  "Provider-level summary for the selected role and metric."
)
#
# Code for HCPCS Code Summary:
div(
  class = "small text-muted mb-2",
  "Code or code-group summary based on the selected grouping level."
)
# Validation:
# - Table rendering stays unchanged.

# Suggestion 11: Add empty-state validation to slow/primary outputs
# Why:
# - If filters produce no rows, charts/tables should show a clear message instead
#   of blank outputs or downstream warnings.
# Where:
# - server.R
# - At the top of renderHighchart/renderDataTable blocks that use current_claims(),
#   enriched_current_claims(), provider_summary(), or code_summary()
# - Server code; tiny UI-supporting snippet only
# Code:
validate(
  need(
    nrow(current_claims()) > 0,
    "No records match the current filters. Try broadening the date, provider, or HCPCS selections."
  )
)
# For provider outputs, use:
validate(
  need(
    nrow(provider_summary()) > 0,
    "No providers match the current filters."
  )
)
# For code outputs, use:
validate(
  need(
    nrow(code_summary()) > 0,
    "No HCPCS/CPT codes match the current filters."
  )
)
# Validation:
# - Test with an intentionally narrow filter that returns zero rows.
# - Confirm charts/tables show readable messages.

# Suggestion 12: Make download limit language visible but compact
# Why:
# - Data View already communicates a 1,000-row download limit. This version keeps
#   the note concise and puts "CSV" and "1,000" where users scan first.
# Where:
# - ui.R
# - Data View card, near downloadButton("download_claims_preview")
# - UI code
# Code:
div(
  class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
  downloadButton(
    outputId = "download_claims_preview",
    label = "Download CSV"
  ),
  span(
    class = "small text-muted",
    "CSV export is limited to the first 1,000 filtered records."
  )
)
# Validation:
# - Button still triggers existing downloadHandler.

# Suggestion 13: Add a tiny app-wide spacing style for stacked cards
# Why:
# - Most layout is already good. A small spacing helper can prevent stacked cards
#   from feeling cramped without redesigning the page.
# Where:
# - ui.R
# - Near the top of page_sidebar(), after window_title or before sidebar
# - CSS/theme code
# Code:
tags$style(HTML(
  "
  .bslib-card {
    margin-bottom: 0.75rem;
  }
  .value-box {
    min-height: 7rem;
  }
"
))
# Validation:
# - Check Home, analysis tabs, and Data View on desktop and narrow browser widths.
# - Remove or reduce if bslib's default spacing already looks better.

# Suggestion 14: Improve accessibility labels for icon-heavy headers
# Why:
# - Icons are decorative in card headers. Marking them hidden from screen readers
#   can reduce redundant announcements.
# Where:
# - ui.R
# - Any card_header div(icon(...), "Title")
# - UI code
# Code:
card_header(
  div(
    icon("table", `aria-hidden` = "true"),
    "Data Preview"
  )
)
# Validation:
# - Visual appearance should not change.
# - Apply opportunistically rather than rewriting every header at once.

# Suggestion 15: Optional nav labels if tabs become crowded
# Why:
# - The nav labels are understandable, but "HCPCS Code Analysis" is long. If tabs
#   crowd on smaller screens, a shorter label can improve readability.
# Where:
# - ui.R
# - nav_panel title only
# - UI code
# Code:
# Replace:
#   nav_panel(title = "HCPCS Code Analysis", ...)
#
# With:
nav_panel(title = "Code Analysis")
# Validation:
# - Only use this if the nav bar feels crowded.
# - Keep card headers using "HCPCS/CPT" language for clarity.

# Suggestion 16: Add one-line "what this tab answers" captions
# Why:
# - Each analysis tab has good controls, but a one-line purpose statement helps
#   users choose the right tab.
# Where:
# - ui.R
# - At the top of Provider Analysis and HCPCS Code Analysis nav_panel bodies,
#   before the controls/metrics layout_columns
# - UI code
# Code for Provider Analysis:
div(
  class = "small text-muted mb-2",
  "Explore provider spending patterns, claim volume, and rankings within the current global filter context."
)
#
# Code for HCPCS Code Analysis:
div(
  class = "small text-muted mb-2",
  "Explore HCPCS/CPT code spending, volume, and provider participation within the current global filter context."
)
# Validation:
# - Captions should not push charts too far down the page.

# Suggestion 17: Prefer consistent capitalization in labels
# Why:
# - The UI alternates between sentence case and title case. Small consistency
#   tweaks make the app feel more polished.
# Where:
# - ui.R
# - Input labels and card titles
# - UI code
# Code:
# Suggested label style:
# - "Payment months" instead of "Payment Month(s)"
# - "Provider role" or "Match selected providers as" consistently
# - "Top providers" and "Top groups" consistently
# - "Paid per claim line" instead of mixed "Paid / Line" variants
sliderTextInput(
  inputId = "month_range",
  label = "Payment months",
  choices = format(sort(month_map$ClaimMonthDate), "%b %Y"),
  selected = format(
    c(
      min(month_map$ClaimMonthDate),
      max(month_map$ClaimMonthDate)
    ),
    "%b %Y"
  )
)
# Validation:
# - This is wording-only; inputId stays the same.

# Suggestion 18: Add concise chart subtitles in server renderHighchart outputs
# Why:
# - Subtitles clarify global-vs-local filter context without adding more UI
#   chrome around each chart.
# Where:
# - server.R
# - Inside renderHighchart outputs after hc_title(text = NULL)
# - Server/UI-supporting chart code
# Code:
hc_subtitle(
  text = "Based on current global filters and tab-local controls"
)
# Validation:
# - Confirm subtitle does not crowd chart on narrow screens.

# Suggestion 19: Major UI direction to defer, not implement now
# Why:
# - A full navigation redesign or dashboard shell could eventually help, but the
#   current app already has a functional bslib layout. A broad redesign would be
#   higher-risk and distract from analysis improvements.
# Where:
# - Future design round only
# Recommendation:
# - Defer any full app-shell redesign until after Provider Analysis, HCPCS Code
#   Analysis, Data View, and map workflows are stable.

# Prioritized implementation checklist
# 1. Highest-impact, lowest-risk changes
# - Rename sidebar heading to "Global Filters" and add the short sidebar note.
# - Add tab-local control captions to Provider and HCPCS analysis accordions.
# - Add empty-state validate/need messages to charts and tables.
# - Tighten map instruction text.
# - Add table captions to Data View, Provider Summary, and Code Summary.
#
# 2. Medium-risk polish
# - Add Home page context note.
# - Rename "Providers Paid" to "Billing Providers".
# - Add chart subtitles in highcharter outputs.
# - Standardize label capitalization.
# - Add compact Data View download wording.
#
# 3. Optional later improvements
# - Add the small spacing CSS if pages feel cramped.
# - Add accessibility attributes to decorative icons opportunistically.
# - Shorten "HCPCS Code Analysis" to "Code Analysis" only if nav tabs feel crowded.
# - Revisit a broader app-shell redesign only after core workflows stabilize.

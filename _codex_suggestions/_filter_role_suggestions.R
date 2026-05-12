# Suggested code chunks for adding billing/servicing role selection.
# Copy these chunks manually into the existing app files if you want to apply them.
# Existing app objects identified:
# - UI file: ui.R
# - Server file: server.R
# - Claims dataset object: claims
# - Claims filtering reactive: current_claims
# - Individual provider datamods result: current_providers()
# - Organization provider datamods result: current_organizations()
# - Billing NPI claim column: BillingProvider
# - Servicing NPI claim column: ServicingProvider


# 1. UI chunk: individual provider role selector
# Copy into ui.R inside the "Individual Providers" accordion_panel, near the existing
# provider_search_context radioButtons. This suggests the new role selector while
# leaving existing controls untouched until you manually decide what to replace.
checkboxGroupInput(
  inputId = "individual_provider_roles",
  label = "Provider Role",
  choices = c("Billing" = "billing", "Servicing" = "servicing"),
  selected = c("billing", "servicing"),
  inline = TRUE
)


# 2. UI chunk: organization provider role selector
# Copy into ui.R inside the "Organizations" accordion_panel, near the existing
# org_search_context radioButtons. This suggests the new role selector while
# leaving existing controls untouched until you manually decide what to replace.
checkboxGroupInput(
  inputId = "organization_provider_roles",
  label = "Provider Role",
  choices = c("Billing" = "billing", "Servicing" = "servicing"),
  selected = c("billing", "servicing"),
  inline = TRUE
)


# 3. Server chunk: selected individual NPIs
# Copy into server.R after current_providers is defined and before current_claims
# needs these values. If you place this below current_claims, Shiny can still
# resolve it at runtime, but keeping helper reactives nearby is easier to read.
# Uses the existing datamods reactive current_providers().
selected_individual_npis <- reactive({
  current_providers()$NPI
})


# 4. Server chunk: selected organization NPIs
# Copy into server.R after current_organizations is defined and before current_claims
# needs these values. Uses the existing datamods reactive current_organizations().
selected_organization_npis <- reactive({
  current_organizations()$NPI
})


# 5. Server chunk: build selected billing and servicing NPI vectors
# Copy into server.R near the selected_*_npis helpers above.
# These combine selected individual and organization NPIs according to the role
# checkboxes. If neither checkbox is selected for a provider section, that section
# contributes no NPIs for that role.
provider_filters_active <- reactive({
  length(input$individual_provider_roles) < 2 ||
    length(input$organization_provider_roles) < 2 ||
    length(selected_individual_npis()) < nrow(providers) ||
    length(selected_organization_npis()) < nrow(organizations)
})

selected_billing_npis <- reactive({
  c(
    if ("billing" %in% input$individual_provider_roles) {
      selected_individual_npis()
    },
    if ("billing" %in% input$organization_provider_roles) {
      selected_organization_npis()
    }
  )
})

selected_servicing_npis <- reactive({
  c(
    if ("servicing" %in% input$individual_provider_roles) {
      selected_individual_npis()
    },
    if ("servicing" %in% input$organization_provider_roles) {
      selected_organization_npis()
    }
  )
})


# 6. Server chunk: minimal replacement for provider filtering in current_claims
# Copy into server.R inside current_claims, after temp_claims is created from the
# date and HCPCS filters. Replace the existing provider_search_context /
# org_search_context conditional provider-filter block with this chunk.
# This uses the existing claims columns BillingProvider and ServicingProvider.
if (!provider_filters_active()) {
  temp_claims
} else {
  temp_claims |>
    filter(
      BillingProvider %in% selected_billing_npis() |
        ServicingProvider %in% selected_servicing_npis()
    )
}


# 7. Manual validation checklist
# - Confirm ui.R shows both new checkbox groups in the expected accordion panels.
# - Confirm both role selectors default to Billing and Servicing selected.
# - Confirm no provider role selected means claims are not restricted by provider role.
# - Confirm individual Billing only filters BillingProvider against current_providers()$NPI.
# - Confirm individual Servicing only filters ServicingProvider against current_providers()$NPI.
# - Confirm organization Billing only filters BillingProvider against current_organizations()$NPI.
# - Confirm organization Servicing only filters ServicingProvider against current_organizations()$NPI.
# - Confirm selecting both individual and organization roles keeps rows matching either role.


# ---- Round 2: refine provider role filtering logic ----

# Replace the first-round provider_filters_active, selected_billing_npis, and
# selected_servicing_npis helpers with this section.
# Keep the existing selected_individual_npis and selected_organization_npis
# helpers from round 1.
#
# These helpers separate four ideas:
# - whether the individual provider datamods rows are narrowed
# - whether the organization provider datamods rows are narrowed
# - whether the individual provider role checkbox has any selected role
# - whether the organization provider role checkbox has any selected role
#
# This assumes checkboxGroupInput values are c("billing", "servicing"). If your
# UI currently uses c("Billing", "Servicing"), either change these string checks
# to match that UI or update the UI choices to use lowercase values.
individual_provider_rows_narrowed <- reactive({
  length(selected_individual_npis()) < nrow(providers)
})

organization_provider_rows_narrowed <- reactive({
  length(selected_organization_npis()) < nrow(organizations)
})

individual_provider_roles_selected <- reactive({
  length(input$individual_provider_roles) > 0
})

organization_provider_roles_selected <- reactive({
  length(input$organization_provider_roles) > 0
})

individual_provider_filter_active <- reactive({
  individual_provider_rows_narrowed() &&
    individual_provider_roles_selected()
})

organization_provider_filter_active <- reactive({
  organization_provider_rows_narrowed() &&
    organization_provider_roles_selected()
})

provider_filters_active <- reactive({
  individual_provider_filter_active() ||
    organization_provider_filter_active()
})


# Replacement/refinement for selected_billing_npis and selected_servicing_npis.
# These group-specific helpers preserve enough context for current_claims to use
# OR when one effective provider group is active and AND when billing and
# servicing constraints come from different active provider groups.
individual_billing_npis <- reactive({
  if (
    individual_provider_filter_active() &&
      "billing" %in% input$individual_provider_roles
  ) {
    selected_individual_npis()
  } else {
    character(0)
  }
})

individual_servicing_npis <- reactive({
  if (
    individual_provider_filter_active() &&
      "servicing" %in% input$individual_provider_roles
  ) {
    selected_individual_npis()
  } else {
    character(0)
  }
})

organization_billing_npis <- reactive({
  if (
    organization_provider_filter_active() &&
      "billing" %in% input$organization_provider_roles
  ) {
    selected_organization_npis()
  } else {
    character(0)
  }
})

organization_servicing_npis <- reactive({
  if (
    organization_provider_filter_active() &&
      "servicing" %in% input$organization_provider_roles
  ) {
    selected_organization_npis()
  } else {
    character(0)
  }
})

# Optional compatibility helpers if other app code already refers to these names.
# The replacement current_claims block below intentionally uses the group-specific
# helpers instead of relying only on these combined vectors.
selected_billing_npis <- reactive({
  c(individual_billing_npis(), organization_billing_npis())
})

selected_servicing_npis <- reactive({
  c(individual_servicing_npis(), organization_servicing_npis())
})


# Replace the first-round provider-filtering block inside current_claims with
# this block. Copy it after temp_claims is created from date and HCPCS filters.
#
# What it does:
# - no narrowed provider section + selected role: return temp_claims unchanged
# - only individual providers active: allow selected NPIs as billing, servicing,
#   or either, depending on the individual role checkboxes
# - only organizations active: same behavior for selected organization NPIs
# - both provider sections active: apply billing constraints to BillingProvider
#   and servicing constraints to ServicingProvider; when both role-specific
#   constraints exist, both are required
if (!provider_filters_active()) {
  temp_claims
} else if (
  individual_provider_filter_active() &&
    !organization_provider_filter_active()
) {
  temp_claims |>
    filter(
      BillingProvider %in% individual_billing_npis() |
        ServicingProvider %in% individual_servicing_npis()
    )
} else if (
  organization_provider_filter_active() &&
    !individual_provider_filter_active()
) {
  temp_claims |>
    filter(
      BillingProvider %in% organization_billing_npis() |
        ServicingProvider %in% organization_servicing_npis()
    )
} else {
  billing_constraints_active <- length(selected_billing_npis()) > 0
  servicing_constraints_active <- length(selected_servicing_npis()) > 0

  temp_claims |>
    filter(
      (!billing_constraints_active |
        BillingProvider %in% selected_billing_npis()),
      (!servicing_constraints_active |
        ServicingProvider %in% selected_servicing_npis())
    )
}


# Round 2 manual validation checklist
# - With no datamods provider/org narrowing, confirm provider role checkbox
#   changes alone do not restrict claims.
# - Narrow individual providers and select Billing only: claims should match
#   BillingProvider only.
# - Narrow individual providers and select Servicing only: claims should match
#   ServicingProvider only.
# - Narrow individual providers and select both roles: claims may match either
#   BillingProvider or ServicingProvider.
# - Narrow organizations as Billing and individuals as Servicing: claims should
#   require BillingProvider in selected organization NPIs AND ServicingProvider
#   in selected individual NPIs.
# - Narrow individuals as Billing and organizations as Servicing: claims should
#   require BillingProvider in selected individual NPIs AND ServicingProvider in
#   selected organization NPIs.
# - Clear all roles for one provider section: that section should contribute no
#   provider constraint.


# ---- Round 3: evaluate cross-filter dynamic choice narrowing ----

# Short assessment:
# Dynamic cross-filter narrowing is practical in principle because the app already
# has clean lookup tables and a claims table with the needed join keys:
# - claims$HCPCSCode
# - claims$BillingProvider
# - claims$ServicingProvider
# - claims$ClaimMonth
#
# However, making all three datamods sections narrow each other at the same time
# is not a small low-risk change. If current_codes depends on available_hcpcs_data,
# available_hcpcs_data depends on current_providers, current_providers depends on
# available_providers_data, and available_providers_data depends on current_codes,
# that creates a circular reactive dependency.
#
# Recommendation: use a staged implementation. Start with one-way narrowing, then
# add the next direction only after validating that no reactive cycle is created.


# Current datamods source data objects:
# - Individual providers: data = reactive(providers)
# - Organizations: data = reactive(organizations)
# - HCPCS lookup: data = reactive(hcpcs_lookup)
#
# These are static lookup data frames wrapped in reactive expressions. They do
# not currently change based on other filter sections.


# Smallest viable architecture:
# - Keep the UI sections separate.
# - Do not create one giant joined claims table.
# - Create "available rows" reactives for each datamods section.
# - Each available rows reactive should be based on claims filtered by OTHER
#   sections, not by itself.
# - Avoid enabling all bidirectional dependencies in one edit.


# Suggested insertion point:
# Copy candidate helper reactives into server.R after the Round 2 provider role
# helpers are defined and before the select_group_server calls that will use
# them. To do that cleanly, the datamods calls may need to move below these
# helpers. If moving current_codes/current_providers/current_organizations feels
# too invasive, use the staged fallback below instead.


# Candidate helper chunk: claims used for provider choices
# Intended use: constrain available individual providers by date, selected HCPCS,
# and active organization filters, but not by selected individual providers.
# WARNING: This references current_codes() and current_organizations(), so do not
# simultaneously make current_codes/current_organizations depend on provider
# choice data without carefully avoiding reactive cycles.
claims_for_provider_choices <- reactive({
  temp_claims <-
    claims |>
    filter(ClaimMonth %in% current_date_range()$ClaimMonth)

  if (exists("current_codes")) {
    temp_claims <-
      temp_claims |>
      filter(HCPCSCode %in% current_codes()$Code)
  }

  if (organization_provider_filter_active()) {
    temp_claims <-
      temp_claims |>
      filter(
        BillingProvider %in% organization_billing_npis() |
          ServicingProvider %in% organization_servicing_npis()
      )
  }

  temp_claims
})

available_provider_npis <- reactive({
  unique(c(
    claims_for_provider_choices()$BillingProvider,
    claims_for_provider_choices()$ServicingProvider
  ))
})

available_providers_data <- reactive({
  providers |>
    filter(NPI %in% available_provider_npis())
})


# Candidate helper chunk: claims used for organization choices
# Intended use: constrain available organizations by date, selected HCPCS, and
# active individual provider filters, but not by selected organizations.
claims_for_organization_choices <- reactive({
  temp_claims <-
    claims |>
    filter(ClaimMonth %in% current_date_range()$ClaimMonth)

  if (exists("current_codes")) {
    temp_claims <-
      temp_claims |>
      filter(HCPCSCode %in% current_codes()$Code)
  }

  if (individual_provider_filter_active()) {
    temp_claims <-
      temp_claims |>
      filter(
        BillingProvider %in% individual_billing_npis() |
          ServicingProvider %in% individual_servicing_npis()
      )
  }

  temp_claims
})

available_organization_npis <- reactive({
  unique(c(
    claims_for_organization_choices()$BillingProvider,
    claims_for_organization_choices()$ServicingProvider
  ))
})

available_organizations_data <- reactive({
  organizations |>
    filter(NPI %in% available_organization_npis())
})


# Candidate helper chunk: claims used for HCPCS choices
# Intended use: constrain available HCPCS codes by date and active provider/org
# filters, but not by selected HCPCS values themselves.
claims_for_hcpcs_choices <- reactive({
  temp_claims <-
    claims |>
    filter(ClaimMonth %in% current_date_range()$ClaimMonth)

  if (!provider_filters_active()) {
    temp_claims
  } else if (
    individual_provider_filter_active() &&
      !organization_provider_filter_active()
  ) {
    temp_claims |>
      filter(
        BillingProvider %in% individual_billing_npis() |
          ServicingProvider %in% individual_servicing_npis()
      )
  } else if (
    organization_provider_filter_active() &&
      !individual_provider_filter_active()
  ) {
    temp_claims |>
      filter(
        BillingProvider %in% organization_billing_npis() |
          ServicingProvider %in% organization_servicing_npis()
      )
  } else {
    billing_constraints_active <- length(selected_billing_npis()) > 0
    servicing_constraints_active <- length(selected_servicing_npis()) > 0

    temp_claims |>
      filter(
        (!billing_constraints_active |
          BillingProvider %in% selected_billing_npis()),
        (!servicing_constraints_active |
          ServicingProvider %in% selected_servicing_npis())
      )
  }
})

available_hcpcs_codes <- reactive({
  unique(claims_for_hcpcs_choices()$HCPCSCode)
})

available_hcpcs_data <- reactive({
  hcpcs_lookup |>
    filter(Code %in% available_hcpcs_codes())
})


# Suggested datamods updates:
# Replace only the data argument in each existing select_group_server call.
# Keep the existing id and vars values unchanged.
#
# Individual providers:
#   data = available_providers_data
#
# Organizations:
#   data = available_organizations_data
#
# HCPCS lookup:
#   data = available_hcpcs_data
#
# Do not apply all three replacements at once unless you have verified the
# dependency graph does not loop. In the current app shape, applying all three
# direct replacements is likely to create a reactive cycle.


# Circular dependency warning:
# The safe rule is: a datamods output should not depend on an available_*_data
# reactive that directly or indirectly calls that same datamods output.
#
# Examples of cycles to avoid:
# - current_codes() -> available_hcpcs_data() -> current_providers() ->
#   available_providers_data() -> current_codes()
# - current_providers() -> available_providers_data() -> current_organizations()
#   -> available_organizations_data() -> current_providers()
#
# Avoid this by implementing one direction at a time, or by separating "committed"
# filter values from live datamods outputs before using them to update choices.


# Fallback recommendation 1: staged dynamic narrowing
# Stage A, lowest risk:
# - Implement only HCPCS/date -> provider and organization narrowing.
# - Update provider and organization select_group_server data arguments to:
#     data = available_providers_data
#     data = available_organizations_data
# - In this stage, claims_for_provider_choices and
#   claims_for_organization_choices should use date and current_codes only.
#
# Stage B:
# - Add provider/org -> HCPCS narrowing after Stage A is stable.
# - Watch carefully for a current_codes/current_providers dependency cycle.
#
# Stage C:
# - Add individual provider <-> organization narrowing last.
# - Validate billing/servicing role combinations separately.


# Fallback recommendation 2: non-narrowing summaries
# If dynamic datamods narrowing feels too invasive, keep datamods choices static
# and add small summary outputs near each filter section instead, for example:
# - "42 individual providers available after current HCPCS/date filters"
# - "18 organizations available after current provider/date filters"
# - "127 HCPCS codes available after current provider/date filters"
#
# This avoids reactive choice-update complexity while still guiding users toward
# meaningful filters. It is also easier to test because the summaries can use the
# same claims_for_*_choices helpers without feeding those helpers back into
# select_group_server.


# Round 3 manual validation checklist
# - Confirm the app still launches before changing any datamods data arguments.
# - Stage A: select HCPCS/date filters and confirm provider/org choices narrow.
# - Stage A: clear HCPCS/date filters and confirm provider/org choices restore.
# - Stage B: select provider/org filters and confirm HCPCS choices narrow.
# - Stage B: clear provider/org filters and confirm HCPCS choices restore.
# - Stage C: select individual providers and confirm organization choices narrow.
# - Stage C: select organizations and confirm individual provider choices narrow.
# - Test Billing only, Servicing only, and both-role selections for each section.
# - Watch the R console for reactive cycle errors or repeated invalidation loops.
# - Confirm current_claims returns the same results as Round 2 when no dynamic
#   choice narrowing is enabled.


# ---- Round 4: one-way provider-to-HCPCS dynamic narrowing ----

# This round is intentionally one-way:
# - individual provider / organization filters -> HCPCS choices
# - HCPCS choices do not update individual provider / organization choices
#
# This avoids the circular dependency risk from Round 3 while supporting the
# intended workflow: choose providers first, then choose from the codes available
# for those providers.


# Current objects identified:
# - HCPCS lookup object: hcpcs_lookup
# - HCPCS datamods result object: current_codes
# - Individual provider datamods result object: current_providers
# - Organization datamods result object: current_organizations
# - Claims object: claims
# - Claims code column: HCPCSCode
# - Claims provider columns: BillingProvider, ServicingProvider
# - HCPCS select_group_server currently receives: data = reactive(hcpcs_lookup)


# Suggested insertion point:
# Copy these helper reactives into server.R after the Round 2 provider role
# helpers are defined:
# - individual_provider_filter_active
# - organization_provider_filter_active
# - provider_filters_active
# - individual_billing_npis
# - individual_servicing_npis
# - organization_billing_npis
# - organization_servicing_npis
# - selected_billing_npis
# - selected_servicing_npis
#
# The helpers below must not call current_codes(). current_codes should consume
# available_hcpcs_lookup, not the other way around.

claims_for_hcpcs_choices <- reactive({
  # If no provider or organization filters are active, available_hcpcs_lookup()
  # returns the full hcpcs_lookup table. This reactive is only used when provider
  # filters are active.
  temp_claims <-
    claims |>
    filter(ClaimMonth %in% current_date_range()$ClaimMonth)

  if (
    individual_provider_filter_active() &&
      !organization_provider_filter_active()
  ) {
    temp_claims |>
      filter(
        BillingProvider %in% individual_billing_npis() |
          ServicingProvider %in% individual_servicing_npis()
      )
  } else if (
    organization_provider_filter_active() &&
      !individual_provider_filter_active()
  ) {
    temp_claims |>
      filter(
        BillingProvider %in% organization_billing_npis() |
          ServicingProvider %in% organization_servicing_npis()
      )
  } else {
    billing_constraints_active <- length(selected_billing_npis()) > 0
    servicing_constraints_active <- length(selected_servicing_npis()) > 0

    temp_claims |>
      filter(
        (!billing_constraints_active |
          BillingProvider %in% selected_billing_npis()),
        (!servicing_constraints_active |
          ServicingProvider %in% selected_servicing_npis())
      )
  }
})

available_hcpcs_codes <- reactive({
  unique(claims_for_hcpcs_choices()$HCPCSCode)
})

available_hcpcs_lookup <- reactive({
  if (!provider_filters_active()) {
    hcpcs_lookup
  } else {
    hcpcs_lookup |>
      filter(Code %in% available_hcpcs_codes())
  }
})


# Date/month note:
# The helper above includes current_date_range() when provider filters are active,
# so HCPCS choices reflect both the selected providers and selected months. If you
# want HCPCS availability to ignore month selection, remove this line from
# claims_for_hcpcs_choices():
#
#   filter(ClaimMonth %in% current_date_range()$ClaimMonth)
#
# The no-provider-filter case intentionally returns the full hcpcs_lookup table,
# matching the requirement that all HCPCS choices appear when no provider filters
# are active.


# Minimal replacement for the existing HCPCS datamods::select_group_server call:
# Replace only the data argument in the current_codes block.
#
# Existing:
#   data = reactive(hcpcs_lookup),
#
# Replace with:
#   data = available_hcpcs_lookup,
#
# Keep the existing id and vars unchanged:
current_codes <-
  select_group_server(
    id = "codes",
    data = available_hcpcs_lookup,
    vars = reactive(c(
      "Type",
      "Category",
      "Subcategory",
      "Family",
      "MajorProcedureIndicator",
      "CodeDescription"
    ))
  )


# Placement note for the current_codes block:
# In the current server.R, current_codes is defined before current_providers and
# the Round 2 provider role helpers. Because available_hcpcs_lookup depends on
# those provider helpers, the safest copy-paste change is:
# - keep current_date_range near the top
# - define current_providers and current_organizations
# - define the Round 2 provider role helpers
# - define claims_for_hcpcs_choices, available_hcpcs_codes, available_hcpcs_lookup
# - then define current_codes using data = available_hcpcs_lookup
# - keep current_claims after current_codes
#
# This changes server object order only; it does not require UI/layout changes.


# Circular dependency guard:
# Do not use current_codes() inside claims_for_hcpcs_choices,
# available_hcpcs_codes, or available_hcpcs_lookup. current_claims can continue
# to use current_codes() after the user selects HCPCS values.
#
# Also do not change provider or organization select_group_server calls to depend
# on current_codes(). They should remain:
#   data = reactive(providers)
#   data = reactive(organizations)


# Round 4 manual validation checklist
# - With no provider filters active, all HCPCS choices appear.
# - With individual providers filtered as Billing only, HCPCS choices narrow to
#   codes where BillingProvider is in individual_billing_npis().
# - With individual providers filtered as Servicing only, HCPCS choices narrow to
#   codes where ServicingProvider is in individual_servicing_npis().
# - With individual providers filtered as Billing and Servicing, HCPCS choices
#   include codes where either provider role matches.
# - With organization Billing and individual Servicing active, HCPCS choices
#   narrow to codes matching BillingProvider in organization_billing_npis() AND
#   ServicingProvider in individual_servicing_npis().
# - Selecting HCPCS codes does not change provider or organization filter choices.
# - current_claims still applies HCPCS filtering after HCPCS selections are made.
# - Clearing provider and organization filters restores the full HCPCS lookup.

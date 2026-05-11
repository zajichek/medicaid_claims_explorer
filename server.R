# Created: 2026-05-05
# Author: Alex Zajichek
# Project: Medicaid Claims Explorer
# Description: Handles user interactions

server <-
  function(input, output, session) {
    ## Extract the selected date range
    current_date_range <-
      reactive({
        # Extract the current selected range
        temp_range <- input$month_range

        # Extract date ranges
        temp_min_date <- month_map$ClaimMonthDate[
          month_map$ClaimMonthYear == temp_range[1]
        ]
        temp_max_date <- month_map$ClaimMonthDate[
          month_map$ClaimMonthYear == temp_range[2]
        ]

        # Return the filter month map to capture full range
        month_map |>
          filter(
            ClaimMonthDate >= temp_min_date,
            ClaimMonthDate <= temp_max_date
          )
      })

    ## Extract the selected codes
    current_codes <-
      # Filters the dataset at once
      select_group_server(
        id = "codes",
        data = reactive(hcpcs_lookup),
        vars = reactive(c(
          "Type",
          "Category",
          "Subcategory",
          "Family",
          "MajorProcedureIndicator",
          "CodeDescription"
        ))
      )

    ## Extract the selected (type I) providers
    current_providers <-
      # Filters the dataset at once
      select_group_server(
        id = "providers",
        data = reactive(providers),
        vars = reactive(c(
          "NPI",
          "LastName",
          "FirstName",
          "Credentials",
          "TaxonomyCode",
          "City",
          "Zip",
          "Sex"
        ))
      )
    # Keep list of NPI's
    selected_individual_npis <- reactive({
      current_providers()$NPI
    })

    ## Extract the selected organizations (type II providers)
    current_organizations <-
      # Filters the dataset at once
      select_group_server(
        id = "organizations",
        data = reactive(organizations),
        vars = reactive(c(
          "NPI",
          "Name",
          "TaxonomyCode",
          "Subpart",
          "City",
          "Zip"
        ))
      )
    # Keep list of NPI's
    selected_organization_npis <- reactive({
      current_organizations()$NPI
    })

    ## Establish provider search context based on input
    # Have selections been made?
    individual_provider_rows_narrowed <- reactive({
      length(selected_individual_npis()) < nrow(providers)
    })
    organization_provider_rows_narrowed <- reactive({
      length(selected_organization_npis()) < nrow(organizations)
    })
    # Are the roles activated?
    individual_provider_roles_selected <- reactive({
      length(input$individual_provider_roles) > 0
    })
    organization_provider_roles_selected <- reactive({
      length(input$organization_provider_roles) > 0
    })
    # Are the filters active?
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

    ## Establish the active sets of selected NPI's (accounting for search context)
    # Individual (Type I)
    individual_billing_npis <- reactive({
      if (
        individual_provider_filter_active() &&
          "Billing" %in% input$individual_provider_roles
      ) {
        selected_individual_npis()
      } else {
        character(0)
      }
    })
    individual_servicing_npis <- reactive({
      if (
        individual_provider_filter_active() &&
          "Servicing" %in% input$individual_provider_roles
      ) {
        selected_individual_npis()
      } else {
        character(0)
      }
    })
    # Organizations (Type II)
    organization_billing_npis <- reactive({
      if (
        organization_provider_filter_active() &&
          "Billing" %in% input$organization_provider_roles
      ) {
        selected_organization_npis()
      } else {
        character(0)
      }
    })
    organization_servicing_npis <- reactive({
      if (
        organization_provider_filter_active() &&
          "Servicing" %in% input$organization_provider_roles
      ) {
        selected_organization_npis()
      } else {
        character(0)
      }
    })

    ## Establish the final selected sets of billing/servicing providers
    selected_billing_npis <- reactive({
      c(individual_billing_npis(), organization_billing_npis())
    })

    selected_servicing_npis <- reactive({
      c(individual_servicing_npis(), organization_servicing_npis())
    })

    # Selected claims records
    current_claims <-
      reactive({
        # Filter on date + code filters
        temp_claims <-
          claims |>
          filter(
            ClaimMonth %in% current_date_range()$ClaimMonth,
            HCPCSCode %in% current_codes()$Code
          )

        ## Check for + apply provider filters
        if (!provider_filters_active()) {
          temp_claims
        } else if (
          individual_provider_filter_active() &&
            !organization_provider_filter_active()
        ) {
          temp_claims |>
            filter(
              BillingProvider %in%
                individual_billing_npis() |
                ServicingProvider %in% individual_servicing_npis()
            )
        } else if (
          organization_provider_filter_active() &&
            !individual_provider_filter_active()
        ) {
          temp_claims |>
            filter(
              BillingProvider %in%
                organization_billing_npis() |
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

    # Display claim count
    output$claim_row_count <-
      renderText({
        # Count the number of claim rows retrieved
        nrow(current_claims())
      })

    # Show data
    output$claims_table <- DT::renderDataTable({
      current_claims() |> sample_n(min(1000, nrow(current_claims())))
    })
  }

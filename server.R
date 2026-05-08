# Created: 2026-05-05
# Author: Alex Zajichek
# Project: Medicaid Claims Explorer
# Description: Handles user interactions

server <-
  function(input, output, session) {
    # Selected date range
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

    # Selected codes
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

    # Selected providers
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

    # Selected organizations
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

    # Display the total payment amounts for the selected NPI's
    output$total_spending <-
      renderText({
        claims |>

          # Filter to claims relevant to any of the selected providers
          filter(
            BillingProvider %in%
              c(current_providers()$NPI, current_organizations()$NPI) |
              ServicingProvider %in%
                c(current_providers()$NPI, current_organizations()$NPI)
          ) |>

          # Compute total
          summarize(
            PaidAmount = sum(PaidAmount)
          ) |>

          # Extract it
          pull("PaidAmount")
      })

    # Show data
    output$provider_table <- DT::renderDataTable({
      current_providers()
    })

    # Display the map contents
    output$utilization_map <- renderLeaflet({
      base_map
    })
  }

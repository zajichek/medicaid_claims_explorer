# Created: 2026-05-05
# Author: Alex Zajichek
# Project: Medicaid Claims Explorer
# Description: Handles user interactions

server <-
  function(input, output, session) {
    # Selected providers
    current_providers <-
      # Filters the dataset at once
      select_group_server(
        id = "providers",
        data = reactive(providers),
        vars = reactive(c(
          "NPI",
          "EntityType",
          "Organization",
          "LastName",
          "FirstName",
          "City",
          "Zip"
        ))
      )

    # Show data
    output$provider_table <- DT::renderDataTable({
      current_providers() |>
        select(
          NPI,
          `Entity Type` = EntityType,
          Organization,
          `Last Name` = LastName,
          `First Name` = FirstName,
          Address,
          City,
          Zip
        )
    })

    # Display the map contents
    output$utilization_map <- renderLeaflet({
      base_map
    })
  }

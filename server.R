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
    # Have selections been made in the filters?
    individual_provider_rows_narrowed <- reactive({
      length(selected_individual_npis()) < nrow(providers)
    })
    organization_provider_rows_narrowed <- reactive({
      length(selected_organization_npis()) < nrow(organizations)
    })
    # Are the roles activated in the checkboxes?
    individual_provider_roles_selected <- reactive({
      length(input$individual_provider_roles) > 0
    })
    organization_provider_roles_selected <- reactive({
      length(input$organization_provider_roles) > 0
    })
    # Are the filters active (rows filtered + context activated)?
    individual_provider_filter_active <- reactive({
      individual_provider_rows_narrowed() &&
        individual_provider_roles_selected()
    })
    organization_provider_filter_active <- reactive({
      organization_provider_rows_narrowed() &&
        organization_provider_roles_selected()
    })
    # Are any filters active (individual or organization)?
    provider_filters_active <- reactive({
      individual_provider_filter_active() ||
        organization_provider_filter_active()
    })

    ## Establish the active sets of selected NPI's (accounting for search context)
    # Individual (Type I)
    individual_billing_npis <- reactive({
      if (
        # Type I providers are selected AND "Billing" is marked
        individual_provider_filter_active() &&
          "Billing" %in% input$individual_provider_roles
      ) {
        # Return the currently selected Type I providers
        selected_individual_npis()
      } else {
        character(0)
      }
    })
    individual_servicing_npis <- reactive({
      if (
        # Type I providers are selected AND "Servicing" is marked
        individual_provider_filter_active() &&
          "Servicing" %in% input$individual_provider_roles
      ) {
        # Return the currently selected Type I providers
        selected_individual_npis()
      } else {
        character(0)
      }
    })
    # Organizations (Type II)
    organization_billing_npis <- reactive({
      if (
        # Type II providers are selected AND "Billing" is marked
        organization_provider_filter_active() &&
          "Billing" %in% input$organization_provider_roles
      ) {
        # Return the currently selected Type II providers
        selected_organization_npis()
      } else {
        character(0)
      }
    })
    organization_servicing_npis <- reactive({
      if (
        # Type II providers are selected AND "Servicing" is marked
        organization_provider_filter_active() &&
          "Servicing" %in% input$organization_provider_roles
      ) {
        # Return the currently selected Type II providers
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

    ## Extract set of claims to select from for current provider selection

    # Find base set of claim records
    claims_for_hcpcs_choices <-
      reactive({
        # Filter on datefilters
        temp_claims <-
          claims |>
          filter(ClaimMonth %in% current_date_range()$ClaimMonth)

        ## Check for + apply provider filters
        if (!provider_filters_active()) {
          temp_claims
        } else if (
          # Search only on Type I providers
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
          # Search only on Type II providers
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
          # Check for active filter on each context
          billing_constraints_active <- length(selected_billing_npis()) > 0
          servicing_constraints_active <- length(selected_servicing_npis()) > 0

          # Only keep rows with matching billing AND servicing provider (intersection) if both active
          temp_claims |>
            filter(
              (!billing_constraints_active |
                BillingProvider %in% selected_billing_npis()),
              (!servicing_constraints_active |
                ServicingProvider %in% selected_servicing_npis())
            )
        }
      })

    # Extract unique codes
    available_hcpcs_codes <- reactive({
      unique(claims_for_hcpcs_choices()$HCPCSCode)
    })

    # Return final lookup table
    available_hcpcs_lookup <- reactive({
      hcpcs_lookup |>
        filter(Code %in% available_hcpcs_codes())
    })

    ## Extract the selected codes
    current_codes <-
      # Filters the dataset at once
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

    # Selected claims records
    current_claims <-
      reactive({
        claims_for_hcpcs_choices() |>
          filter(
            HCPCSCode %in% current_codes()$Code
          )
      })

    ## Data view helpers
    # Base providers for data viewing
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
    # Enriched data
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

    ## Provider-analysis objects
    # Copy of enriched data
    provider_analysis_claims <- reactive({
      enriched_current_claims()
    })
    # Summarize provider data based on selected context
    provider_summary <- reactive({
      req(input$pa_provider_role)

      if (input$pa_provider_role == "billing") {
        temp_provider_analysis_claims_summary <-
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
        temp_provider_analysis_claims_summary <-
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
      }
      temp_provider_analysis_claims_summary |>
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
    # More provider summary helpers
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

    ###### Home page

    ### Display KPI's
    # Total spend
    output$total_spend <-
      renderUI({
        div(
          scales::dollar(sum(current_claims()$PaidAmount)),
          br(),
          span(
            paste0(
              scales::dollar(
                sum(current_claims()$PaidAmount) /
                  sum(current_claims()$ClaimLines)
              ),
              " per claim line"
            ),
            style = "font-size: 12px"
          )
        )
      })
    # Providers paid
    output$billing_provider_count <-
      renderUI({
        div(
          n_distinct(current_claims()$BillingProvider),
          br(),
          span(
            paste0(
              n_distinct(current_claims()$ServicingProvider),
              " servicing providers"
            ),
            style = "font-size: 12px"
          )
        )
      })
    # Claim lines
    output$claim_lines <-
      renderUI({
        div(
          format(sum(current_claims()$ClaimLines), big.mark = ","),
          br(),
          span(
            paste0(n_distinct(current_codes()$Code), " distinct codes"),
            style = "font-size: 12px"
          )
        )
      })

    ### Plots
    # Spend over time
    output$spend_over_time <-
      renderHighchart({
        current_claims() |>

          # Compute total by month
          summarize(
            TotalSpend = sum(PaidAmount),
            .by = ClaimMonth
          ) |>

          # Join to get the date object
          inner_join(
            y = current_date_range(),
            by = "ClaimMonth"
          ) |>
          arrange(ClaimMonthDate) |>

          # Make the plot
          hchart(
            "line",
            hcaes(
              x = ClaimMonthDate,
              y = TotalSpend
            ),
            marker = list(enabled = TRUE)
          ) |>
          hc_xAxis(
            title = list(text = "Month")
          ) |>
          hc_yAxis(
            title = list(text = "Total Paid ($)")
          ) |>
          hc_tooltip(
            pointFormat = "Paid: <b>${point.y:,.0f}</b>"
          )
      })

    ### Map
    # Provider data for map
    map_provider_data <- reactive({
      provider_lookup <-
        bind_rows(
          providers |>
            transmute(
              NPI,
              ProviderName = paste(FirstName, LastName),
              ProviderType = "Individual",
              Zip,
              lon,
              lat
            ),
          organizations |>
            transmute(
              NPI,
              ProviderName = Name,
              ProviderType = "Organization",
              Zip,
              lon,
              lat
            )
        )

      current_claims() |>
        inner_join(
          y = provider_lookup,
          by = c("BillingProvider" = "NPI")
        ) |>
        filter(!is.na(lon), !is.na(lat)) |>
        summarize(
          ProviderName = first(ProviderName),
          ProviderType = first(ProviderType),
          Zip = first(Zip),
          lon = jitter(first(lon), factor = .005),
          lat = jitter(first(lat), factor = .005),
          ServicingProviders = n_distinct(ServicingProvider),
          Codes = n_distinct(HCPCSCode),
          ClaimLines = sum(ClaimLines),
          TotalSpend = sum(PaidAmount),
          # Patients = sum(Patients), # Optional future field if available.
          .by = BillingProvider
        ) |>
        rename(NPI = BillingProvider)
    })
    # Extract map zoom level
    current_map_zoom <- reactive({
      if (is.null(input$county_map_zoom)) {
        7
      } else {
        input$county_map_zoom
      }
    })
    # Generic metric (in case we change it)
    current_map_metric <- reactive({
      "TotalSpend"
      # Future examples:
      # input$map_metric
      # "ClaimLines"
      # "BillingProviders"
    })
    # Palette for heatmap
    metric_palette <- function(data, metric) {
      colorNumeric(
        palette = "YlOrRd",
        domain = data[[metric]],
        na.color = "#cccccc"
      )
    }
    metric_radius <- function(values, min_radius = 4, max_radius = 18) {
      if (length(values) == 0 || all(is.na(values))) {
        return(numeric(0))
      }

      value_range <- range(values, na.rm = TRUE)

      if (diff(value_range) == 0) {
        return(rep((min_radius + max_radius) / 2, length(values)))
      }

      min_radius +
        (values - value_range[1]) /
          diff(value_range) *
          (max_radius - min_radius)
    }

    ## Map build
    # Show map contents
    output$county_map <- renderLeaflet({
      base_map
    })
    # Update with data
    observeEvent(
      list(
        map_provider_data(),
        current_map_metric()
      ),
      {
        metric <- current_map_metric()
        temp_provider_data <- map_provider_data()
        pal <- metric_palette(temp_provider_data, metric)

        temp_provider_data <-
          temp_provider_data |>
          mutate(
            MapMetricValue = .data[[metric]],
            MapColor = pal(MapMetricValue),
            MapRadius = metric_radius(
              MapMetricValue,
              min_radius = 3,
              max_radius = 10
            ),
            ClusterMetric = MapMetricValue,
            ClusterClaimLines = ClaimLines
            # ClusterPatients = Patients # Optional future field if available.
          )

        leafletProxy("county_map") |>
          clearGroup("provider_markers") |>
          addCircleMarkers(
            data = temp_provider_data,
            lng = ~lon,
            lat = ~lat,
            group = "provider_markers",
            label = ~ paste0(
              ProviderName,
              " | ",
              scales::dollar(TotalSpend)
            ),
            popup = ~ paste0(
              "Provider: ",
              ProviderName,
              "<br>NPI: ",
              NPI,
              "<br>Type: ",
              ProviderType,
              "<br>ZIP Code: ",
              Zip,
              "<br>Total Spend: ",
              scales::dollar(TotalSpend),
              "<br>Claim Lines: ",
              format(ClaimLines, big.mark = ","),
              "<br>Spend Per Claim Line: ",
              scales::dollar(TotalSpend / ClaimLines),
              "<br>Servicing Providers: ",
              ServicingProviders,
              "<br>Distinct HCPCS Codes: ",
              Codes
            ),
            options = markerOptions(
              metric = ~ClusterMetric,
              claimLines = ~ClusterClaimLines
              # patients = ~ClusterPatients # Optional future field if available.
            ),
            color = ~MapColor,
            fillColor = ~MapColor,
            radius = ~MapRadius,
            fillOpacity = 0.85,
            opacity = 0.85,
            weight = 1,
            clusterOptions = markerClusterOptions(
              showCoverageOnHover = FALSE,
              spiderfyOnMaxZoom = TRUE,
              zoomToBoundsOnClick = TRUE,
              disableClusteringAtZoom = 14,
              maxClusterRadius = 75,
              iconCreateFunction = cluster_icon_js
            )
          )
      },
      ignoreInit = FALSE
    )

    ### Provider Analysis

    # Metric cards
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

    # Bubble chart
    output$pa_provider_scatter <- renderHighchart({
      chart_data <-
        provider_summary() |>
        mutate(
          SelectedMetric = .data[[input$pa_metric]],
          BubbleSize = pmax(Patients, 1),
          TooltipText = paste0(
            "<b>",
            ProviderLabel,
            "</b>",
            "<br>NPI: ",
            NPI,
            "<br>Type: ",
            ProviderType,
            "<br>Location: ",
            City,
            ", ",
            State,
            " ",
            Zip,
            "<br>Total Paid: ",
            scales::dollar(PaidAmount),
            "<br>Claim Lines: ",
            format(ClaimLines, big.mark = ","),
            "<br>Patients: ",
            format(Patients, big.mark = ","),
            "<br>Paid / Claim Line: ",
            scales::dollar(PaidPerClaimLine),
            "<br>Distinct HCPCS: ",
            DistinctHCPCS
          )
        )

      hchart(
        chart_data,
        "bubble",
        hcaes(
          x = ClaimLines,
          y = SelectedMetric,
          size = BubbleSize,
          name = ProviderLabel,
          color = ProviderType
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

    # Bar chart
    output$pa_top_provider_bar <- renderHighchart({
      chart_data <-
        provider_top_summary() |>
        arrange(SelectedMetric) |>
        mutate(
          ProviderLabel = forcats::fct_inorder(ProviderLabel),
          TooltipText = paste0(
            "<b>",
            ProviderLabel,
            "</b>",
            "<br>NPI: ",
            NPI,
            "<br>Type: ",
            ProviderType,
            "<br>",
            provider_metric_label(),
            ": ",
            if (input$pa_metric %in% c("PaidAmount", "PaidPerClaimLine")) {
              scales::dollar(SelectedMetric)
            } else {
              format(SelectedMetric, big.mark = ",")
            },
            "<br>Total Paid: ",
            scales::dollar(PaidAmount),
            "<br>Claim Lines: ",
            format(ClaimLines, big.mark = ","),
            "<br>Patients: ",
            format(Patients, big.mark = ",")
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

    # Table
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

    ### Data View
    # Metric cards
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
    # Show data in table
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
    # Download the data
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

    ## Additional data summaries (not currently used)
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
          .by = c(
            ServicingProvider,
            ServicingProviderName,
            ServicingProviderType
          )
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
  }

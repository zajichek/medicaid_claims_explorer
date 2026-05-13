# ---- Round 1: hierarchical Leaflet map suggestions ----

# Assessment of current map structure:
# - Leaflet UI output is defined in ui.R as:
#     leafletOutput(outputId = "county_map")
# - Leaflet rendering is defined in server.R as:
#     output$county_map <- renderLeaflet({ base_map })
# - The current map output id is:
#     "county_map"
# - The current ZIP-level map data reactive is:
#     total_spend_by_zip
# - The current filtered claims reactive is:
#     current_claims
# - leafletProxy is already used:
#     leafletProxy("county_map")
# - Available coordinate columns are:
#     lon, lat
# - Available ZIP columns are:
#     Zip
# - Available provider identifier/name columns include:
#     providers: NPI, FirstName, LastName, Zip, lon, lat
#     organizations: NPI, Name, Zip, lon, lat
#
# Current behavior is already close to the right update pattern: the app renders
# base_map once, then uses leafletProxy() to update markers. The main improvement
# is to make marker detail conditional on zoom level so the app does not try to
# show roughly 14,000 provider-level points when zoomed out.

# Recommended incremental strategy:
# - Keep the existing ZIP-level map working.
# - Add a provider-level map data reactive.
# - Add zoom-aware switching using input$county_map_zoom.
# - Continue using leafletProxy("county_map") instead of rebuilding the widget.
# - Show provider-level points only at high zoom.
# - In Round 1, do not build county/region aggregation unless a clean county data
#   field already exists in the app data. Use ZIP-level points for zoomed-out and
#   medium zoom views.

# Suggested zoom thresholds:
# - zoom < 8: ZIP-level summary for now
# - zoom 8-10: ZIP-level summary
# - zoom >= 11: provider-level points
#
# If a county/region field is added later, zoom < 8 could switch to that grouped
# layer. Do not add that in Round 1.

# Suggested helper chunk 1: alias or replace the existing ZIP data reactive.
# Copy into server.R near the existing total_spend_by_zip reactive.
# This keeps the existing total_spend_by_zip working and gives the map observer a
# clearer, map-specific name.
map_zip_data <- reactive({
  total_spend_by_zip()
})


# Suggested helper chunk 2: provider-level map data.
# Copy into server.R after total_spend_by_zip/map_zip_data and before the map
# observer.
#
# This creates one row per billing provider in current_claims(), joined to the
# existing provider and organization lookup tables for ZIP centroid coordinates.
# It intentionally uses current_claims(), so provider-level points are limited to
# the current filters.
#
# If you later want servicing-provider points instead, replace BillingProvider
# with ServicingProvider in the join and grouping below.
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
      lon = first(lon),
      lat = first(lat),
      ServicingProviders = n_distinct(ServicingProvider),
      Codes = n_distinct(HCPCSCode),
      ClaimLines = sum(ClaimLines),
      TotalSpend = sum(PaidAmount),
      .by = BillingProvider
    ) |>
    rename(NPI = BillingProvider)
})


# Suggested helper chunk 3: current zoom and active map level.
# Copy into server.R near the map data reactives.
# input$county_map_zoom is provided by Leaflet/Shiny after the map initializes,
# so use a default zoom before it exists.
current_map_zoom <- reactive({
  if (is.null(input$county_map_zoom)) {
    7
  } else {
    input$county_map_zoom
  }
})

active_map_level <- reactive({
  if (current_map_zoom() >= 11) {
    "provider"
  } else {
    "zip"
  }
})


# Suggested observer chunk: zoom-aware leafletProxy updates.
# Replace the current observe({ ... leafletProxy("county_map") ... }) map update
# block in server.R with this observer.
#
# What this does:
# - keeps output$county_map <- renderLeaflet({ base_map }) unchanged
# - clears only marker layers, not polygons/tiles
# - adds ZIP markers when active_map_level() is "zip"
# - adds provider markers when active_map_level() is "provider"
# - avoids setView() on every filter update, so user zoom/pan is preserved
observeEvent(
  list(
    map_zip_data(),
    map_provider_data(),
    current_map_zoom(),
    active_map_level()
  ),
  {
    proxy <-
      leafletProxy("county_map") |>
      clearGroup("zip_markers") |>
      clearGroup("provider_markers")

    if (active_map_level() == "zip") {
      temp_zip_data <- map_zip_data()

      pal <-
        colorNumeric(
          palette = "RdYlGn",
          domain = -1 * sort(unique(temp_zip_data$TotalSpend))
        )

      proxy |>
        addCircleMarkers(
          data = temp_zip_data,
          lng = ~lon,
          lat = ~lat,
          group = "zip_markers",
          label = ~ paste0(Zip, " (click for info)"),
          popup = ~ paste0(
            "ZIP Code: ",
            Zip,
            "<br>Total Spend: ",
            scales::dollar(TotalSpend),
            "<br>Claim Lines: ",
            format(ClaimLines, big.mark = ","),
            "<br>Spend Per Claim Line: ",
            scales::dollar(TotalSpend / ClaimLines),
            "<br>Billing Providers: ",
            BillingProviders,
            "<br>Servicing Providers: ",
            ServicingProviders,
            "<br>Distinct HCPCS Codes: ",
            Codes
          ),
          color = ~ pal(-1 * TotalSpend),
          radius = ~ pmax(4, scale(TotalSpend)[, 1] + 5),
          fillOpacity = 0.85,
          stroke = FALSE
        )
    } else {
      temp_provider_data <- map_provider_data()

      proxy |>
        addCircleMarkers(
          data = temp_provider_data,
          lng = ~lon,
          lat = ~lat,
          group = "provider_markers",
          label = ~ paste0(ProviderName, " (", NPI, ")"),
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
            "<br>Servicing Providers: ",
            ServicingProviders,
            "<br>Distinct HCPCS Codes: ",
            Codes
          ),
          radius = 3,
          color = "#1f78b4",
          fillOpacity = 0.65,
          stroke = FALSE,
          clusterOptions = markerClusterOptions()
        )
    }
  },
  ignoreInit = FALSE
)

# Optional first-load centering chunk.
# The current map observer calls setView() every time data changes, which can
# fight with user zooming/panning. Prefer setting the initial view once in
# output$county_map instead:
#
# output$county_map <- renderLeaflet({
#   base_map |>
#     setView(lng = -89.7, lat = 44.8, zoom = 7)
# })
#
# Keep this optional; the existing base_map may already be acceptable.

# Performance safeguards:
# - Use addCircleMarkers(), not regular marker icons.
# - Keep provider radius small, such as radius = 3.
# - Keep provider popups simple.
# - Use clusterOptions = markerClusterOptions() for provider-level points.
# - Provider points should come from current_claims(), not the full provider
#   lookup, so filters keep the marker count lower.
# - Avoid rebuilding the whole Leaflet widget on every filter change.
# - Avoid expensive joins inside the map observer. Do joins in map_provider_data()
#   and map_zip_data(), then let the observer only render layers.
# - If provider-level plotting is still slow, add a temporary guard such as:
#
#   req(nrow(map_provider_data()) <= 5000)
#
#   or show provider markers only at zoom >= 12.

# Popup/label examples:
# - ZIP popup:
#     ZIP Code, Total Spend, Claim Lines, Spend Per Claim Line,
#     Billing Providers, Servicing Providers, Distinct HCPCS Codes.
# - Provider popup:
#     Provider Name, NPI, Type, ZIP Code, Total Spend, Claim Lines,
#     Servicing Providers, Distinct HCPCS Codes.

# Fallback implementation if zoom-aware switching is too invasive:
# - Keep the existing ZIP-level map as the default.
# - Add provider-level markers only when the user zooms high enough, but do not
#   remove ZIP markers yet.
# - Use clusterOptions = markerClusterOptions() on provider markers.
# - If even that feels too much, skip provider markers in Round 1 and only
#   preserve the existing ZIP map while removing repeated setView() calls.

# Manual validation checklist:
# - App initially loads map without provider-level markers.
# - At medium zoom, ZIP-level points display.
# - At high zoom, provider-level points display.
# - Zooming back out removes provider-level markers.
# - Filters update the map without rebuilding the whole UI.
# - Provider-level plotting remains responsive.
# - Popups show expected ZIP and provider fields.
# - No app files were edited by Codex.

# ---- Round 2: multi-level clustering + metric-aware visual encoding ----

# Assessment of current Round 1 structure:
# - The map now has two clear levels:
#     active_map_level() == "zip"
#     active_map_level() == "provider"
# - ZIP markers are drawn in group = "zip_markers".
# - Provider markers are drawn in group = "provider_markers".
# - Provider markers already use:
#     clusterOptions = markerClusterOptions()
# - leafletProxy("county_map") is already used, which is the right foundation.
# - Current map metric columns include:
#     TotalSpend, ClaimLines, BillingProviders, ServicingProviders, Codes
# - A patient count column was not identified in the current map data. Treat
#   patients as a future metric only after a concrete column is added upstream.
#
# Explicit ZIP-prefix hierarchy is practical but not the best first Round 2
# change. Wisconsin ZIPs usually start with "53", "54", and a few neighboring
# prefixes, so 2-digit ZIP aggregation would be very coarse. 3-digit ZIP groups
# could help, but they add another layer, another popup format, and another
# transition to validate. The simplest high-value improvement is metric-aware
# color/radius styling for both ZIP and provider markers, plus slightly better
# provider cluster behavior.

# Recommended hierarchy strategy:
# Option A, recommended first:
# - Keep the current ZIP -> provider transition.
# - Keep provider marker clustering.
# - Add consistent metric-based color and radius encoding to ZIP and provider
#   markers.
# - Improve cluster behavior with lightweight markerClusterOptions settings.
#
# Option B, optional later:
# - Add a 3-digit ZIP aggregation layer between ZIP and provider markers.
# - This is reasonable if users still feel the jump from ZIP to clustered
#   providers is too abrupt.
#
# Option C, not recommended yet:
# - Add explicit 2-digit and 3-digit ZIP hierarchy immediately.
# - This is more code and validation for limited first-pass value.

# Suggested zoom thresholds and layer transitions:
# - zoom < 8: ZIP-level markers, larger radius range, lower opacity
# - zoom 8-10: ZIP-level markers, normal radius range
# - zoom 11-13: provider-level clustered markers
# - zoom >= 14: provider-level clustered markers with smaller cluster radius
#
# In Round 2, keep active_map_level() as-is unless you add ZIP3. The visual
# encoding alone will make high-spend ZIPs/providers visible across levels.

# Suggested helper chunk 1: selectable metric.
# Copy near current_map_zoom / active_map_level.
#
# This uses a fixed default now. Later, if you add a UI control, replace the
# hard-coded "TotalSpend" with input$map_metric.
current_map_metric <- reactive({
  "TotalSpend"
  # Future examples:
  # input$map_metric
  # "ClaimLines"
  # "BillingProviders"
})


# Suggested helper chunk 2: metric palette and radius scaling.
# Copy near current_map_metric.
#
# These helpers are intentionally small and Leaflet-native. They work for either
# ZIP data or provider data as long as the selected metric column exists.
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


# Suggested ZIP marker styling replacement:
# In the active_map_level() == "zip" branch, replace the existing pal and radius
# logic with this metric-aware version.
metric <- current_map_metric()
temp_zip_data <- map_zip_data()
pal <- metric_palette(temp_zip_data, metric)
temp_zip_data <-
  temp_zip_data |>
  mutate(
    MapMetricValue = .data[[metric]],
    MapColor = pal(MapMetricValue),
    MapRadius = metric_radius(MapMetricValue, min_radius = 5, max_radius = 20)
  )

leafletProxy("county_map") |>
  addCircleMarkers(
    data = temp_zip_data,
    lng = ~lon,
    lat = ~lat,
    group = "zip_markers",
    label = ~ paste0(
      Zip,
      ": ",
      scales::dollar(TotalSpend)
    ),
    popup = ~ paste0(
      "ZIP Code: ",
      Zip,
      "<br>Total Spend: ",
      scales::dollar(TotalSpend),
      "<br>Claim Lines: ",
      format(ClaimLines, big.mark = ","),
      "<br>Spend Per Claim Line: ",
      scales::dollar(TotalSpend / ClaimLines),
      "<br>Billing Providers: ",
      BillingProviders,
      "<br>Servicing Providers: ",
      ServicingProviders,
      "<br>Distinct HCPCS Codes: ",
      Codes
    ),
    color = ~MapColor,
    fillColor = ~MapColor,
    radius = ~MapRadius,
    fillOpacity = 0.75,
    opacity = 0.9,
    weight = 1
  )


# Suggested provider marker styling replacement:
# In the provider branch, replace the fixed radius/color with metric-aware
# styling. Keep provider markers smaller than ZIP markers.
metric <- current_map_metric()
temp_provider_data <- map_provider_data()
pal <- metric_palette(temp_provider_data, metric)
temp_provider_data <-
  temp_provider_data |>
  mutate(
    MapMetricValue = .data[[metric]],
    MapColor = pal(MapMetricValue),
    MapRadius = metric_radius(MapMetricValue, min_radius = 3, max_radius = 10)
  )

leafletProxy("county_map") |>
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
    color = ~MapColor,
    fillColor = ~MapColor,
    radius = ~MapRadius,
    fillOpacity = 0.65,
    opacity = 0.85,
    weight = 1,
    clusterOptions = markerClusterOptions(
      showCoverageOnHover = FALSE,
      spiderfyOnMaxZoom = TRUE,
      disableClusteringAtZoom = 14,
      maxClusterRadius = 45
    )
  )


# Palette examples:
# - "YlOrRd": low values are light yellow, high values are warm red. Good for
#   spend intensity.
# - "Blues": calmer sequential palette if red feels too alarming.
# - "Viridis": perceptually balanced if leaflet/scales support it in your setup.
#
# For spending, start with:
#   palette = "YlOrRd"
#
# For claim line volume, either "YlGnBu" or "Blues" can work well.

# Optional helper chunk: lightweight 3-digit ZIP aggregation.
# Only add this if the ZIP -> provider jump still feels too abrupt after
# metric-based styling. This is a small intermediate layer, but it does require
# one more active_map_level value and one more observer branch.
map_zip3_data <- reactive({
  map_zip_data() |>
    mutate(Zip3 = substr(Zip, 1, 3)) |>
    summarize(
      BillingProviders = sum(BillingProviders),
      ServicingProviders = sum(ServicingProviders),
      Codes = sum(Codes),
      ClaimLines = sum(ClaimLines),
      TotalSpend = sum(TotalSpend),
      lon = mean(lon, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      .by = Zip3
    )
})

# If using map_zip3_data(), update active_map_level() like this:
active_map_level <- reactive({
  if (current_map_zoom() >= 11) {
    "provider"
  } else if (current_map_zoom() >= 8) {
    "zip"
  } else {
    "zip3"
  }
})

# Then add a "zip3_markers" group to the observer clear step:
#   clearGroup("zip3_markers")
#
# And add a branch similar to ZIP markers, using Zip3 in the label/popup.
#
# Note: Codes should ideally be n_distinct across underlying claims, not sum of
# ZIP-level distinct counts. The summed value is a quick approximation only. If
# exact distinct code counts matter, build map_zip3_data from current_claims()
# directly instead of from map_zip_data().

# Provider cluster summaries:
# True aggregated cluster popups that show total spend/claim lines for the
# current client-side cluster are not straightforward with standard leaflet R
# markerClusterOptions. Clusters are formed in the browser after markers are
# sent, so the server-side data frame does not naturally know which providers
# ended up in each cluster.
#
# Recommended simple alternative:
# - Put the most important metric in each provider marker label.
# - Use metric-aware color/radius so high-spend providers are visible inside and
#   outside clusters.
# - Let marker clusters show count, and use disableClusteringAtZoom = 14 so users
#   can inspect individual providers at high zoom.
#
# If true cluster totals become necessary, consider a later custom JavaScript
# iconCreateFunction. That is possible but is beyond the minimal Round 2 scope.

# Performance safeguards:
# - Keep map_provider_data() as the only provider lookup join; do not repeat that
#   join inside the observer.
# - Keep metric_palette() and metric_radius() cheap and data-frame local.
# - Avoid changing base_map or renderLeaflet() when only markers change.
# - Keep provider marker popups text-only and avoid large HTML widgets in popups.
# - Use disableClusteringAtZoom so Leaflet does less cluster work when zoomed in.
# - If filters produce too many provider markers, increase the provider threshold
#   from zoom >= 11 to zoom >= 12 or 13.
# - If map_zip3_data() is added, compute it reactively once and render from that
#   reactive rather than aggregating in the observer branch.

# Staged implementation recommendation:
# Phase 1:
# - Add current_map_metric, metric_palette, and metric_radius.
# - Apply metric-based color/radius to ZIP and provider markers.
#
# Phase 2:
# - Tune provider markerClusterOptions:
#     showCoverageOnHover = FALSE
#     spiderfyOnMaxZoom = TRUE
#     disableClusteringAtZoom = 14
#     maxClusterRadius = 45
#
# Phase 3:
# - Add optional map_zip3_data only if users still need an intermediate layer.
# - Keep ZIP3 approximate at first, then make distinct counts exact only if the
#   approximation causes confusion.

# Manual validation checklist:
# - Zoom transitions remain responsive.
# - Provider markers visually distinguish high-spend providers.
# - ZIP summaries visually distinguish high-spend ZIPs.
# - Cluster behavior remains performant.
# - Marker colors and radii update when filters change.
# - Map remains readable when zoomed out.
# - Provider labels/popups still show expected fields.
# - ZIP labels/popups still show expected fields.
# - No app files were modified directly.

# ---- Round 3: provider-only clustered map with cluster summaries ----

# Short assessment:
# Now that ZIP-level summaries are no longer desired, the map can be simplified a
# lot. The app no longer needs explicit zoom-level switching between ZIP markers
# and provider markers. Leaflet.markercluster can handle the hierarchy naturally:
# broad clusters when zoomed out, smaller clusters as users zoom in, and
# individual provider markers at high zoom.
#
# Current app objects that can likely be removed or bypassed:
# - total_spend_by_zip
# - map_zip_data
# - active_map_level
# - zoom-based switching between ZIP and provider layers
# - group = "zip_markers"
# - ZIP-specific marker popups and labels
#
# Objects not identified in the current app and therefore not relevant unless
# added later:
# - map_zip2_data
# - map_zip3_data
# - map_zip5_data

# Replacement strategy:
# - Keep one provider-level reactive: map_provider_data.
# - Keep current_map_metric, metric_palette, and metric_radius if already present.
# - Render output$county_map once with base_map.
# - Use one leafletProxy observer that clears and redraws only
#   group = "provider_markers".
# - Use markerClusterOptions() so clusters automatically become more granular as
#   zoom increases.
# - Do not use input$county_map_zoom or active_map_level for layer switching.

# Suggested provider-level data reactive.
# Replace the existing map_provider_data with this version if you want a slightly
# more explicit provider-only shape.
#
# Notes:
# - Uses current_claims(), so markers reflect selected filters.
# - Aggregates by billing provider, matching the current map behavior.
# - Patient count is included only as a commented placeholder because no patient
#   column was identified in the current claims/map data.
# - If you later want servicing-provider geography instead, replace
#   BillingProvider with ServicingProvider in the join and grouping.
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
      lon = first(lon),
      lat = first(lat),
      ServicingProviders = n_distinct(ServicingProvider),
      Codes = n_distinct(HCPCSCode),
      ClaimLines = sum(ClaimLines),
      TotalSpend = sum(PaidAmount),
      # Patients = sum(Patients), # Optional future field if available.
      .by = BillingProvider
    ) |>
    rename(NPI = BillingProvider)
})


# Suggested marker styling preparation.
# Copy this inside the provider map observer before addCircleMarkers().
# It keeps circles readable by bounding radius between 3 and 10 pixels.
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


# Suggested simplified leafletProxy observer.
# Replace the current observeEvent that branches on active_map_level().
#
# What it does:
# - clears only provider markers
# - redraws provider markers when provider data or the selected metric changes
# - keeps the base map intact
# - relies on marker clustering for all zoom-level hierarchy
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
        fillOpacity = 0.65,
        opacity = 0.85,
        weight = 1,
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          spiderfyOnMaxZoom = TRUE,
          zoomToBoundsOnClick = TRUE,
          disableClusteringAtZoom = 14,
          maxClusterRadius = 55
        )
      )
  },
  ignoreInit = FALSE
)


# Cluster-level aggregate summaries: feasibility assessment
# Standard leaflet markerClusterOptions in R easily shows marker counts, but it
# does not automatically know summed TotalSpend or ClaimLines in a way that can
# be used in normal R popups. Clusters are built client-side by
# Leaflet.markercluster after the markers are sent to the browser.
#
# Small feasible option:
# - Store metric values on each marker via markerOptions().
# - Use htmlwidgets::JS in iconCreateFunction to sum values from
#   cluster.getAllChildMarkers().
# - Show provider count plus abbreviated metric total directly in the cluster
#   icon HTML.
#
# Tradeoff:
# - This gives a useful visual cluster summary, but not a rich hover/click popup.
# - Formatting is JavaScript-side and intentionally simple.
# - If you need exact formatted cluster popups with many fields, that becomes a
#   custom JS interaction and is beyond the smallest safe change.

# Optional cluster icon summary.
# This requires htmlwidgets::JS. No new package is needed if htmlwidgets is
# already available through Shiny/htmlwidgets dependencies; otherwise use
# htmlwidgets::JS explicitly without library(htmlwidgets).
#
# Replace the clusterOptions block above with this version if you want cluster
# icons to show marker count plus summed selected metric and claim lines.
clusterOptions = markerClusterOptions(
  showCoverageOnHover = FALSE,
  spiderfyOnMaxZoom = TRUE,
  zoomToBoundsOnClick = TRUE,
  disableClusteringAtZoom = 14,
  maxClusterRadius = 55,
  iconCreateFunction = htmlwidgets::JS(
    "function(cluster) {
      var markers = cluster.getAllChildMarkers();
      var metricTotal = 0;
      var claimLinesTotal = 0;

      markers.forEach(function(marker) {
        metricTotal += Number(marker.options.metric || 0);
        claimLinesTotal += Number(marker.options.claimLines || 0);
      });

      var abbreviatedMetric =
        metricTotal >= 1000000
          ? '$' + (metricTotal / 1000000).toFixed(1) + 'M'
          : '$' + Math.round(metricTotal / 1000) + 'K';

      var html =
        '<div style=\"line-height:1.05;text-align:center;\">' +
          '<strong>' + markers.length + '</strong><br/>' +
          '<span style=\"font-size:10px;\">' + abbreviatedMetric + '</span>' +
        '</div>';

      return new L.DivIcon({
        html: html,
        className: 'marker-cluster marker-cluster-large',
        iconSize: new L.Point(44, 44)
      });
    }"
  )
)

# Alternatives evaluated:
# A. Store metric values in marker options and aggregate them in iconCreateFunction.
#    Recommended if cluster metric summaries are important. It is the smallest
#    feasible cluster-summary option.
#
# B. Use custom cluster icons showing count plus summed metric.
#    Same as A. Keep the icon text short to avoid clutter.
#
# C. Add a Shiny side panel/card that summarizes providers within current bounds.
#    Good future enhancement. Easier to make rich and exact than cluster popups,
#    but it requires UI space and map bounds reactives.
#
# D. Keep cluster count only and rely on metric-colored/radius-scaled provider
#    markers after clusters open.
#    Safest fallback. Recommended if the JS icon customization feels brittle.

# Suggested markerClusterOptions settings:
# - showCoverageOnHover = FALSE
#     Reduces visual clutter while hovering over clusters.
# - spiderfyOnMaxZoom = TRUE
#     Lets overlapping providers separate at max zoom.
# - zoomToBoundsOnClick = TRUE
#     Natural cluster drill-down behavior.
# - disableClusteringAtZoom = 14
#     Shows individual providers at high zoom.
# - maxClusterRadius = 55
#     Larger values create broader clusters when zoomed out; smaller values break
#     clusters apart earlier.
# - iconCreateFunction = htmlwidgets::JS(...)
#     Optional for custom cluster count + metric summaries.

# Optional future enhancement: map bounds summary.
# Later, add a small card beside/above the map that updates when users pan/zoom:
#
# visible_map_bounds <- reactive(input$county_map_bounds)
#
# map_bounds_summary <- reactive({
#   bounds <- visible_map_bounds()
#   req(bounds)
#
#   map_provider_data() |>
#     filter(
#       lat >= bounds$south,
#       lat <= bounds$north,
#       lon >= bounds$west,
#       lon <= bounds$east
#     ) |>
#     summarize(
#       Providers = n(),
#       TotalSpend = sum(TotalSpend),
#       ClaimLines = sum(ClaimLines)
#     )
# })
#
# This is often simpler and more reliable than rich cluster popups because the
# aggregation stays in R/Shiny. Do not add it unless the UI needs the summary.

# Manual validation checklist:
# - No ZIP-level markers appear.
# - Map starts with provider clusters when zoomed out.
# - Clusters break apart as zoom increases.
# - Individual providers appear at high zoom.
# - Marker colors/radii reflect the selected metric.
# - If cluster summaries are implemented, cluster totals match the providers in
#   the cluster within expected rounding.
# - Cluster summaries update when filters change.
# - Map updates when filters change.
# - No full map rebuild occurs unnecessarily.
# - Performance remains acceptable with roughly 14,000 providers.
# - No app files were modified directly.

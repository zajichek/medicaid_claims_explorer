# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer
# Description: Creates and loads global objects accessible to app

# Load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(datamods)
library(highcharter)
library(leaflet)
library(arrow)
library(sf)
library(bsicons)
library(htmlwidgets)
library(htmltools)
library(DT)
library(shinycssloaders)

### Import app assets

# Datasets
providers <- read_rds(file = "data/assets/providers.rds")
organizations <- read_rds(file = "data/assets/organizations.rds")
hcpcs_lookup <- read_rds(file = "data/assets/hcpcs_lookup.rds")
month_map <-
  read_rds(file = "data/assets/month_map.rds") |>

  # Add additional format for app
  mutate(
    ClaimMonthYear = format(ClaimMonthDate, "%b %Y")
  )
claims <- open_dataset("data/assets/claims") |> collect()

# Map components
state_outline <- read_rds(file = "data/assets/state_outline.rds")
county_outlines <- read_rds(file = "data/assets/county_outlines.rds")

### Build base map

# Base map
base_map <-
  leaflet() |>

  # Add geographic tiles
  addTiles() |>

  # Add WI state outline
  addPolygons(
    data = state_outline,
    fillColor = "gray",
    stroke = FALSE
  ) |>

  # Add county outlines
  addPolygons(
    data = county_outlines,
    color = "black",
    fillColor = "black",
    weight = 1,
    opacity = .5,
    fillOpacity = .25,
    highlightOptions = highlightOptions(
      color = "black",
      weight = 3,
      bringToFront = FALSE
    ),
    label = ~NAME
  )

cluster_icon_js <- htmlwidgets::JS(
  "
  function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var metricTotal = 0;
    var claimLinesTotal = 0;

    markers.forEach(function(marker) {
      metricTotal += Number(marker.options.metric || 0);
      claimLinesTotal += Number(marker.options.claimLines || 0);
    });

    var abbreviatedMetric =
      metricTotal >= 1000000000
        ? '$' + (metricTotal / 1000000000).toFixed(1) + 'B'
        : metricTotal >= 1000000
          ? '$' + (metricTotal / 1000000).toFixed(1) + 'M'
          : '$' + Math.round(metricTotal / 1000) + 'K';

    var bgColor =
      metricTotal >= 100000000 ? '#7f0000' :
      metricTotal >= 10000000  ? '#d7301f' :
      metricTotal >= 1000000   ? '#fc8d59' :
                                  '#fdcc8a';

    var textColor =
      metricTotal >= 10000000 ? 'white' : '#222';

    var size =
      metricTotal >= 100000000 ? 76 :
      metricTotal >= 10000000  ? 68 :
      metricTotal >= 1000000   ? 60 :
                                  52;

    var html =
      '<div style=\"' +
        'width:' + size + 'px;' +
        'height:' + size + 'px;' +
        'border-radius:50%;' +
        'background:' + bgColor + ';' +
        'display:flex;' +
        'flex-direction:column;' +
        'align-items:center;' +
        'justify-content:center;' +
        'color:' + textColor + ';' +
        'font-weight:bold;' +
        'line-height:1.05;' +
        'text-align:center;' +
        'white-space:nowrap;' +
        'overflow:visible;' +
        'border:2px solid white;' +
        'box-shadow:0 0 4px rgba(0,0,0,0.45);' +
      '\">' +
        '<strong style=\"font-size:13px;\">' + markers.length + '</strong>' +
        '<span style=\"font-size:10px;\">' + abbreviatedMetric + '</span>' +
      '</div>';

    return new L.DivIcon({
      html: html,
      className: '',
      iconSize: new L.Point(size, size),
      iconAnchor: new L.Point(size / 2, size / 2)
    });
  }
  "
)

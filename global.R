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

# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Extracts and stores geometry objects for app

# Load packages
library(tidyverse)

# State outline
state_outline <-
  maps::map(
    database = "state",
    regions = "wisconsin",
    fill = TRUE,
    plot = FALSE
  )

# County outlines
county_outlines <-
  tigris::counties(cb = TRUE) |>
  filter(
    STATE_NAME == "Wisconsin"
  )

# Zip codes
zips <- tigris::zctas(year = 2010, state = "WI")

# Zip code centroids
zip_centroids <-
  zips |>

  # Get the centroid
  sf::st_centroid() |>

  # Pluck the coordinates
  sf::st_coordinates() |>

  # Make a tibble
  as_tibble() |>

  # Add identifying column
  add_column(
    Zip = zips$ZCTA5CE10
  ) |>

  # Rename columns
  rename(
    lon = X,
    lat = Y
  )

# Write components to file
state_outline |> write_rds(file = "data/assets/state_outline.rds")
county_outlines |> write_rds(file = "data/assets/county_outlines.rds")
zips |> write_rds(file = "data/assets/zips.rds")
zip_centroids |> write_rds(file = "data/assets/zip_centroids.rds")

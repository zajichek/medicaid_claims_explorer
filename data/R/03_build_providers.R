# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Builds the Type 1 provider lookup table

# Load packages
library(DBI)
library(duckdb)
library(tidyverse)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), "data/intermediate/medicaid_database.duckdb")

## Build Type I provider list

# Extract full table
npi_wi <- dbGetQuery(con, "SELECT * FROM npi_wi") |> tibble::as_tibble()

# Import zip code centroids
zip_centroids <- read_rds(file = "data/assets/zip_centroids.rds")

# Keep relevant fields for individual providers
providers <-
  npi_wi |>

  # Filter to Type I
  filter(EntityType == 1) |>

  # Keep some columns
  transmute(
    NPI,
    LastName,
    FirstName,
    Sex,
    Credentials,
    Address,
    City,
    State,
    Zip = str_sub(Zip, 1, 5),
    TaxonomyCode,
    LastUpdateDate,
    DeactivationDate,
    ReactivationDate
  ) |>

  # Join to get zip code coordinates
  left_join(
    y = zip_centroids,
    by = "Zip"
  )

# Write to file
providers |> write_rds(file = "data/assets/providers.rds")

# Disconnect
dbDisconnect(con, shutdown = TRUE)

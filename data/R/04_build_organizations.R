# Created: 2026-05-06
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Builds the Type II provider lookup table

# Load packages
library(DBI)
library(duckdb)
library(tidyverse)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), "data/intermediate/medicaid_database.duckdb")

## Build Type II provider list

# Extract full table
npi_wi <- dbGetQuery(con, "SELECT * FROM npi_wi") |> tibble::as_tibble()

# Import zip code centroids
zip_centroids <- read_rds(file = "data/assets/zip_centroids.rds")

# Keep relevant fields for organizations
organizations <-
  npi_wi |>

  # Filter to Type II
  filter(EntityType == 2) |>

  # Keep some columns
  transmute(
    NPI,
    Name = Organization,
    Address,
    City,
    State,
    Zip = str_sub(Zip, 1, 5),
    TaxonomyCode,
    Subpart = OrganizationSubpart,
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
organizations |> write_rds(file = "data/assets/organizations.rds")

# Disconnect
dbDisconnect(con, shutdown = TRUE)

# Created: 2026-05-05
# Author: Alex Zajichek
# Project: Medicaid Claims Explorer - Data Prep
# Description: Builds the WI provider lookup table

# Load packages
library(DBI)
library(duckdb)
library(tidyverse)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), "data/intermediate/medicaid_database.duckdb")

# Create table in database
dbExecute(
  con,
  '
CREATE TABLE providers AS
SELECT
  "NPI" AS NPI,
  "Entity Type Code" AS EntityType,
  "Provider Organization Name (Legal Business Name)" AS Organization,
  "Provider Last Name (Legal Name)" AS LastName,
  "Provider First Name" AS FirstName,
  "Provider First Line Business Practice Location Address" AS Address,
  "Provider Business Practice Location Address City Name" AS City,
  "Provider Business Practice Location Address State Name" AS State,
  "Provider Business Practice Location Address Postal Code" AS Zip
FROM read_csv_auto("data/raw/npidata_pfile_20050523-20260412.csv")
WHERE "Provider Business Practice Location Address State Name" = \'WI\'
'
)

## Add geocoding for easy mapping

# Extract provider list
providers <- dbGetQuery(con, "SELECT * FROM providers") |> tibble::as_tibble()

# Import zip code centroids
zip_centroids <- read_rds(file = "data/assets/zip_centroids.rds")

# Attach lat/lon
providers <-
  providers |>

  # Keep first 5 only
  mutate(Zip = str_sub(Zip, 1, 5)) |>

  # Join to get location
  left_join(
    y = zip_centroids,
    by = "Zip"
  )

# Write to file
providers |> write_rds(file = "data/assets/providers.rds")

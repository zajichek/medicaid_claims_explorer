# Created: 2026-05-06
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Builds a mapping file for HCPCS codes

# Load packages
library(tidyverse)

# Extract the tables
claims <- read_rds(file = "data/assets/claims.rds")

# Import raw lookup table
hcpcs_lookup <-
  read_csv(file = "data/raw/PPRRVU24_JAN.csv", skip = 9, guess_max = 10000) |>

  # Keep a few columns
  select(
    HCPCSCode = HCPCS,
    HCPCSDescription = DESCRIPTION
  ) |>

  # Unique rows only
  distinct() |>

  # Join to keep existing codes in dataset
  inner_join(
    y = claims |>

      # Keep unique codes
      select(HCPCSCode) |>
      distinct(),
    by = "HCPCSCode"
  ) # ~3114 of 3600 codes in claims dataset accounted for here.

# Write to file
hcpcs_lookup |> write_rds(file = "data/assets/hcpcs_lookup.rds")

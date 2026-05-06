# Created: 2026-05-06
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Filters components to scope of datasets (to reduce size)

# Load packages
library(tidyverse)

# Extract the tables
claims <- read_rds(file = "data/assets/claims.rds")
providers <- read_rds(file = "data/assets/providers.rds")
organizations <- read_rds(file = "data/assets/organizations.rds")

# Filter tables (more)
providers <- providers |>
  filter(NPI %in% claims$BillingProvider | NPI %in% claims$ServicingProvider)
organizations <- organizations |>
  filter(NPI %in% claims$BillingProvider | NPI %in% claims$ServicingProvider)

# Write to file
providers |> write_rds(file = "data/assets/providers.rds")
organizations |> write_rds(file = "data/assets/organizations.rds")

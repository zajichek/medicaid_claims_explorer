# Created: 2026-05-06
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Builds a date mapper from claim records

# Load packages
library(tidyverse)

# Extract the table
claims <- read_rds(file = "data/assets/claims.rds")

# Make a date mapping (for convenience)
month_map <-
  claims |>

  # Keep distinct months
  summarize(
    Records = n(),
    TotalPaidAmount = sum(PaidAmount),
    .by = ClaimMonth
  ) |>

  # Make date mappings
  mutate(
    ClaimMonthDate = parse_date(paste0(ClaimMonth, "-01")),
    ClaimYear = year(ClaimMonthDate),
    ClaimMonthNum = month(ClaimMonthDate),
    ClaimMonthName = month(ClaimMonthDate, label = TRUE, abbr = FALSE)
  )

# Write to file
month_map |> write_rds(file = "data/assets/month_map.rds")

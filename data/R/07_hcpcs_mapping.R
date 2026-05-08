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
  claims |>

  # Keep unique codes
  select(HCPCSCode) |>
  distinct() |>

  # Join to get the descriptor
  left_join(
    y = read_csv(
      file = "data/raw/PPRRVU24_JAN.csv",
      skip = 9,
      guess_max = 10000
    ) |>

      # Keep a few columns
      select(
        HCPCSCode = HCPCS,
        Description = DESCRIPTION
      ) |>

      # Unique rows only
      distinct(),
    by = "HCPCSCode"
  ) |>

  # Join to get BETOS categories
  left_join(
    y = read_csv(file = paste0("data/raw/RBCS_RY_2025.csv")) |>

      # Filter to the latest assignment for each code
      filter(RBCS_Latest_Assignment == 1) |>

      # Keep some columns
      select(
        HCPCSCode = HCPCS_Cd,
        Category = RBCS_Cat_Desc,
        Subcategory = RBCS_Subcat_Desc,
        Family = RBCS_Family_Desc,
        MajorProcedureIndicator = RBCS_Major_Ind
      ),
    by = "HCPCSCode"
  ) |>

  # Classify each code type
  mutate(
    Type = case_when(
      str_detect(HCPCSCode, "^[A-Za-z]") ~ "Level 2", # HCPCS Level II
      TRUE ~ "Level 1 (CPT)" # HCPCS Level I (CPT Codes)
    ),
    CodeDescription = case_when(
      !is.na(Description) ~ paste0(HCPCSCode, " - ", Description),
      TRUE ~ HCPCSCode
    )
  ) |>

  # Rename the column
  rename(Code = HCPCSCode)

# Write to file
hcpcs_lookup |> write_rds(file = "data/assets/hcpcs_lookup.rds")

## Additional analysis

# 1. How many codes of each type are there?
table(hcpcs_lookup$Type)
#   1    2
# 2125 1499

# 2. How many of each type have a descriptor?
table(hcpcs_lookup$Type, !is.na(hcpcs_lookup$Description))
#    FALSE TRUE
#  1   145 1980
#  2   365 1134

# 3. How many of each type have a BETOS category?
table(hcpcs_lookup$Type, !is.na(hcpcs_lookup$Category))
#    FALSE TRUE
#  1   136 1989
#  2   480 1019

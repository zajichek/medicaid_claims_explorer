# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Builds the WI provider lookup table

# Load packages
library(DBI)
library(duckdb)
library(tidyverse)
library(arrow)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), "data/intermediate/medicaid_database.duckdb")

# Attach the full Medicaid spending database
dbExecute(
  con,
  "ATTACH 'data/raw/medicaid-provider-spending.duckdb' AS medicaid_src"
)

# Query payment records to those in our providers set
dbExecute(
  con,
  "
  CREATE TABLE claims AS
  SELECT
    d.BILLING_PROVIDER_NPI_NUM AS BillingProvider,
    d.SERVICING_PROVIDER_NPI_NUM AS ServicingProvider,
    d.HCPCS_CODE AS HCPCSCode,
    d.CLAIM_FROM_MONTH AS ClaimMonth,
    d.TOTAL_PATIENTS AS Patients,
    d.TOTAL_CLAIM_LINES AS ClaimLines,
    d.TOTAL_PAID AS PaidAmount
  FROM medicaid_src.main.dataset d
  INNER JOIN npi_wi n
    ON d.BILLING_PROVIDER_NPI_NUM = n.NPI
  WHERE 
    d.BILLING_PROVIDER_NPI_NUM IS NOT NULL AND 
    d.SERVICING_PROVIDER_NPI_NUM IS NOT NULL
 
  UNION 

  SELECT
    d.BILLING_PROVIDER_NPI_NUM AS BillingProvider,
    d.SERVICING_PROVIDER_NPI_NUM AS ServicingProvider,
    d.HCPCS_CODE AS HCPCSCode,
    d.CLAIM_FROM_MONTH AS ClaimMonth,
    d.TOTAL_PATIENTS AS Patients,
    d.TOTAL_CLAIM_LINES AS ClaimLines,
    d.TOTAL_PAID AS PaidAmount
  FROM medicaid_src.main.dataset d
  INNER JOIN npi_wi n
    ON d.SERVICING_PROVIDER_NPI_NUM = n.NPI
  WHERE 
    d.BILLING_PROVIDER_NPI_NUM IS NOT NULL AND 
    d.SERVICING_PROVIDER_NPI_NUM IS NOT NULL
"
)

# Extract the table
claims <- dbGetQuery(con, "SELECT * FROM claims") |> tibble::as_tibble()

# Write to file
claims |> write_rds(file = "data/assets/claims.rds") # Not tracked by git--local use only

## Write to parquet files by year for app use

# Make directory
dir.create("data/assets/claims", recursive = TRUE, showWarnings = FALSE)

# Iteratively write tables
claims |>

  # Extract the year
  mutate(
    Year = str_sub(ClaimMonth, 1, 4)
  ) |>

  # Nest the data
  nest(.by = Year) |>

  # For each dataset
  mutate(
    files = map2(
      .x = data,
      .y = as.list(Year),
      function(x, y) {
        write_parquet(
          x,
          paste0("data/assets/claims/", y, ".parquet"),
          compression = "zstd"
        )
      }
    )
  )

# Disconnect
dbDisconnect(con, shutdown = TRUE)

# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Loads the master NPI lookup table into temporary DB

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
CREATE TABLE npi_wi AS
SELECT
  CAST("NPI" AS VARCHAR) AS NPI,
  "Entity Type Code" AS EntityType,
  "Provider Organization Name (Legal Business Name)" AS Organization,
  "Provider Last Name (Legal Name)" AS LastName,
  "Provider First Name" AS FirstName,
  "Provider Sex Code" AS Sex,
  "Provider Credential Text" AS Credentials,
  "Provider First Line Business Practice Location Address" AS Address,
  "Provider Business Practice Location Address City Name" AS City,
  "Provider Business Practice Location Address State Name" AS State,
  "Provider Business Practice Location Address Postal Code" AS Zip,
  "Healthcare Provider Taxonomy Code_1" AS TaxonomyCode,
  "Is Organization Subpart" AS OrganizationSubpart,
  "Last Update Date" AS LastUpdateDate,
  "NPI Deactivation Date" AS DeactivationDate,
  "NPI Reactivation Date" AS ReactivationDate
FROM read_csv_auto("data/raw/npidata_pfile_20050523-20260412.csv")
WHERE "Provider Business Practice Location Address State Name" = \'WI\'
'
)

# Disconnect
dbDisconnect(con, shutdown = TRUE)

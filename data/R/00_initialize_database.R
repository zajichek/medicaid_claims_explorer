# Created: 2026-05-05
# Author: Alex Zajichek
# Project: WI Medicaid Claims Explorer - Data Prep
# Description: Creates a local database

# Load packages
library(DBI)
library(duckdb)

# Make the database
con <- dbConnect(duckdb::duckdb(), "data/intermediate/medicaid_database.duckdb")

# Disconnect
dbDisconnect(con, shutdown = TRUE)

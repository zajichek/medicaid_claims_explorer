library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), "medicaid-provider-spending.duckdb")

# List tables
dbListTables(con)

# Peek at a table
dbGetQuery(con, "SELECT * FROM dataset LIMIT 10") |> tibble::as_tibble()

# Get schema info
dbGetQuery(con, "DESCRIBE dataset")

# Total records
dbGetQuery(
  con,
  "SELECT COUNT(1) AS RowCount, SUM(TOTAL_PAID) AS TotalPaid FROM dataset"
)

# Month counts
dbGetQuery(
  con,
  "SELECT CLAIM_FROM_MONTH AS Month, COUNT(1) AS RowCount, SUM(TOTAL_PAID) AS TotalPaid FROM dataset GROUP BY CLAIM_FROM_MONTH ORDER BY CLAIM_FROM_MONTH"
)

## Read from parquet

con2 <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Query directly from parquet
dbGetQuery(
  con2,
  "
  SELECT *
  FROM read_parquet('medicaid-provider-spending.parquet')
  LIMIT 10
"
)

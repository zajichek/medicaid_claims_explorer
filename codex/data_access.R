required_medicaid_columns <- c(
  "BILLING_PROVIDER_NPI_NUM",
  "SERVICING_PROVIDER_NPI_NUM",
  "HCPCS_CODE",
  "CLAIM_FROM_MONTH",
  "TOTAL_PATIENTS",
  "TOTAL_CLAIM_LINES",
  "TOTAL_PAID"
)

connect_medicaid_duckdb <- function(path = "medicaid-provider-spending.duckdb",
                                    read_only = TRUE) {
  if (!file.exists(path)) {
    stop("DuckDB file not found: ", path, call. = FALSE)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
  DBI::dbExecute(con, "SET enable_progress_bar = false")
  con
}

disconnect_medicaid_duckdb <- function(con) {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}

medicaid_source_tbl <- function(con, table = "dataset") {
  available_tables <- DBI::dbListTables(con)

  if (!table %in% available_tables) {
    stop(
      "DuckDB table '", table, "' not found. Available tables: ",
      paste(available_tables, collapse = ", "),
      call. = FALSE
    )
  }

  dplyr::tbl(con, table)
}

validate_medicaid_source <- function(source_tbl) {
  available_columns <- colnames(source_tbl)
  missing_columns <- setdiff(required_medicaid_columns, available_columns)

  if (length(missing_columns) > 0) {
    stop(
      "Source table is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

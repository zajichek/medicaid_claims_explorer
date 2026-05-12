default_result_limit <- 100L
max_result_limit <- 1000L

clean_filter_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")
  }

  trimws(as.character(x[[1]]))
}

bounded_limit <- function(n, default = default_result_limit, max_n = max_result_limit) {
  if (is.null(n) || length(n) == 0 || is.na(n)) {
    return(default)
  }

  as.integer(min(max(n, 1), max_n))
}

collect_standard <- function(query, label = "DuckDB query") {
  start <- Sys.time()
  message("[DuckDB] Starting: ", label)
  data <- dplyr::collect(query)
  elapsed <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 2)
  message("[DuckDB] Finished: ", label, " (", elapsed, "s, ", nrow(data), " rows)")

  if ("HCPCS_CODE" %in% names(data) && !"hcpcs_code" %in% names(data)) {
    names(data)[names(data) == "HCPCS_CODE"] <- "hcpcs_code"
  }

  data
}

limit_result <- function(query, n, default = default_result_limit) {
  utils::head(query, bounded_limit(n, default = default))
}

with_standard_fields <- function(source_tbl) {
  source_tbl |>
    dplyr::mutate(
      claim_month = dbplyr::sql("substr(CAST(CLAIM_FROM_MONTH AS VARCHAR), 1, 7)"),
      billing_provider_npi = dbplyr::sql(
        "COALESCE(NULLIF(TRIM(CAST(BILLING_PROVIDER_NPI_NUM AS VARCHAR)), ''), '[Missing billing NPI]')"
      ),
      servicing_provider_npi = dbplyr::sql(
        "COALESCE(NULLIF(TRIM(CAST(SERVICING_PROVIDER_NPI_NUM AS VARCHAR)), ''), '[Missing servicing NPI]')"
      ),
      hcpcs_code = dbplyr::sql(
        "COALESCE(NULLIF(TRIM(CAST(HCPCS_CODE AS VARCHAR)), ''), '[Missing HCPCS]')"
      )
    )
}

apply_medicaid_filters <- function(source_tbl,
                                   month_start = "",
                                   month_end = "",
                                   hcpcs_code = "",
                                   billing_npi = "",
                                   servicing_npi = "") {
  out <- with_standard_fields(source_tbl)

  month_start <- clean_filter_value(month_start)
  month_end <- clean_filter_value(month_end)
  hcpcs_code <- clean_filter_value(hcpcs_code)
  billing_npi <- clean_filter_value(billing_npi)
  servicing_npi <- clean_filter_value(servicing_npi)

  if (nzchar(month_start)) {
    out <- out |>
      dplyr::filter(.data$claim_month >= month_start)
  }

  if (nzchar(month_end)) {
    out <- out |>
      dplyr::filter(.data$claim_month <= month_end)
  }

  if (nzchar(hcpcs_code)) {
    out <- out |>
      dplyr::filter(.data$hcpcs_code == hcpcs_code)
  }

  if (nzchar(billing_npi)) {
    out <- out |>
      dplyr::filter(.data$billing_provider_npi == billing_npi)
  }

  if (nzchar(servicing_npi)) {
    out <- out |>
      dplyr::filter(.data$servicing_provider_npi == servicing_npi)
  }

  out
}

apply_summary_thresholds <- function(source_tbl,
                                     min_total_paid = NULL,
                                     min_total_patients = NULL) {
  out <- source_tbl

  if (!is.null(min_total_paid) && !is.na(min_total_paid) && min_total_paid > 0) {
    out <- out |>
      dplyr::filter(.data$total_paid >= min_total_paid)
  }

  if (!is.null(min_total_patients) && !is.na(min_total_patients) && min_total_patients > 0) {
    out <- out |>
      dplyr::filter(.data$total_patients >= min_total_patients)
  }

  out
}

add_derived_metrics <- function(source_tbl) {
  source_tbl |>
    dplyr::mutate(
      paid_per_patient = dplyr::if_else(
        .data$total_patients > 0,
        .data$total_paid / .data$total_patients,
        NA_real_
      ),
      paid_per_line = dplyr::if_else(
        .data$claim_lines > 0,
        .data$total_paid / .data$claim_lines,
        NA_real_
      ),
      lines_per_patient = dplyr::if_else(
        .data$total_patients > 0,
        .data$claim_lines / .data$total_patients,
        NA_real_
      )
    )
}

summarise_spending <- function(source_tbl, ...) {
  source_tbl |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      row_count = dplyr::n(),
      total_paid = sum(.data$TOTAL_PAID, na.rm = TRUE),
      total_patients = sum(.data$TOTAL_PATIENTS, na.rm = TRUE),
      claim_lines = sum(.data$TOTAL_CLAIM_LINES, na.rm = TRUE),
      hcpcs_count = dplyr::n_distinct(.data$hcpcs_code),
      billing_provider_count = dplyr::n_distinct(.data$billing_provider_npi),
      servicing_provider_count = dplyr::n_distinct(.data$servicing_provider_npi),
      active_months = dplyr::n_distinct(.data$claim_month),
      .groups = "drop"
    ) |>
    add_derived_metrics()
}

overview_metrics <- function(source_tbl) {
  with_standard_fields(source_tbl) |>
    dplyr::summarise(
      row_count = dplyr::n(),
      total_paid = sum(.data$TOTAL_PAID, na.rm = TRUE),
      total_patients = sum(.data$TOTAL_PATIENTS, na.rm = TRUE),
      total_claim_lines = sum(.data$TOTAL_CLAIM_LINES, na.rm = TRUE),
      hcpcs_count = dplyr::n_distinct(.data$hcpcs_code),
      billing_provider_count = dplyr::n_distinct(.data$billing_provider_npi),
      servicing_provider_count = dplyr::n_distinct(.data$servicing_provider_npi)
    ) |>
    dplyr::mutate(
      paid_per_patient = dplyr::if_else(
        .data$total_patients > 0,
        .data$total_paid / .data$total_patients,
        NA_real_
      ),
      paid_per_line = dplyr::if_else(
        .data$total_claim_lines > 0,
        .data$total_paid / .data$total_claim_lines,
        NA_real_
      ),
      lines_per_patient = dplyr::if_else(
        .data$total_patients > 0,
        .data$total_claim_lines / .data$total_patients,
        NA_real_
      )
    ) |>
    collect_standard("overview_metrics")
}

monthly_trend <- function(source_tbl,
                          month_start = "",
                          month_end = "",
                          hcpcs_code = "",
                          billing_npi = "",
                          servicing_npi = "") {
  apply_medicaid_filters(
    source_tbl,
    month_start = month_start,
    month_end = month_end,
    hcpcs_code = hcpcs_code,
    billing_npi = billing_npi,
    servicing_npi = servicing_npi
  ) |>
    summarise_spending(claim_month = .data$claim_month) |>
    dplyr::arrange(.data$claim_month) |>
    collect_standard("monthly_trend")
}

top_hcpcs <- function(source_tbl, n = 25) {
  apply_medicaid_filters(source_tbl) |>
    summarise_spending(hcpcs_code = .data$hcpcs_code) |>
    dplyr::arrange(dplyr::desc(.data$total_paid)) |>
    limit_result(n, default = 25L) |>
    collect_standard("top_hcpcs")
}

provider_summary <- function(source_tbl,
                             provider_view = c("billing", "servicing"),
                             n = default_result_limit,
                             month_start = "",
                             month_end = "",
                             hcpcs_code = "",
                             billing_npi = "",
                             servicing_npi = "",
                             min_total_paid = NULL,
                             min_total_patients = NULL) {
  provider_view <- match.arg(provider_view)
  provider_col <- if (provider_view == "billing") {
    rlang::sym("billing_provider_npi")
  } else {
    rlang::sym("servicing_provider_npi")
  }

  apply_medicaid_filters(
    source_tbl,
    month_start = month_start,
    month_end = month_end,
    hcpcs_code = hcpcs_code,
    billing_npi = billing_npi,
    servicing_npi = servicing_npi
  ) |>
    summarise_spending(provider_npi = !!provider_col) |>
    apply_summary_thresholds(
      min_total_paid = min_total_paid,
      min_total_patients = min_total_patients
    ) |>
    dplyr::arrange(dplyr::desc(.data$total_paid)) |>
    limit_result(n) |>
    collect_standard(paste0("provider_summary_", provider_view))
}

provider_hcpcs_summary <- function(source_tbl,
                                   provider_npi = "",
                                   provider_view = c("billing", "servicing"),
                                   n = default_result_limit,
                                   month_start = "",
                                   month_end = "",
                                   hcpcs_code = "",
                                   billing_npi = "",
                                   servicing_npi = "",
                                   min_total_paid = NULL,
                                   min_total_patients = NULL) {
  provider_view <- match.arg(provider_view)
  provider_npi <- clean_filter_value(provider_npi)
  provider_col <- if (provider_view == "billing") {
    rlang::sym("billing_provider_npi")
  } else {
    rlang::sym("servicing_provider_npi")
  }

  out <- apply_medicaid_filters(
    source_tbl,
    month_start = month_start,
    month_end = month_end,
    hcpcs_code = hcpcs_code,
    billing_npi = billing_npi,
    servicing_npi = servicing_npi
  )

  if (nzchar(provider_npi)) {
    out <- out |>
      dplyr::filter(!!provider_col == provider_npi)
  }

  out |>
    summarise_spending(provider_npi = !!provider_col, hcpcs_code = .data$hcpcs_code) |>
    apply_summary_thresholds(
      min_total_paid = min_total_paid,
      min_total_patients = min_total_patients
    ) |>
    dplyr::arrange(dplyr::desc(.data$total_paid)) |>
    limit_result(n) |>
    collect_standard(paste0("provider_hcpcs_summary_", provider_view))
}

provider_monthly_trend <- function(source_tbl,
                                   provider_npi,
                                   provider_view = c("billing", "servicing"),
                                   month_start = "",
                                   month_end = "",
                                   hcpcs_code = "",
                                   billing_npi = "",
                                   servicing_npi = "",
                                   min_total_paid = NULL,
                                   min_total_patients = NULL) {
  provider_view <- match.arg(provider_view)
  provider_npi <- clean_filter_value(provider_npi)
  provider_col <- if (provider_view == "billing") {
    rlang::sym("billing_provider_npi")
  } else {
    rlang::sym("servicing_provider_npi")
  }

  if (!nzchar(provider_npi)) {
    return(tibble::tibble())
  }

  apply_medicaid_filters(
    source_tbl,
    month_start = month_start,
    month_end = month_end,
    hcpcs_code = hcpcs_code,
    billing_npi = billing_npi,
    servicing_npi = servicing_npi
  ) |>
    dplyr::filter(!!provider_col == provider_npi) |>
    summarise_spending(claim_month = .data$claim_month) |>
    dplyr::arrange(.data$claim_month) |>
    collect_standard(paste0("provider_monthly_trend_", provider_view))
}

top_billing_providers <- function(source_tbl, n = 25) {
  provider_summary(source_tbl, provider_view = "billing", n = n)
}

provider_screening_summary <- function(source_tbl, n = default_result_limit) {
  provider_summary(source_tbl, provider_view = "billing", n = n)
}

available_months <- function(source_tbl) {
  with_standard_fields(source_tbl) |>
    dplyr::distinct(.data$claim_month) |>
    dplyr::arrange(.data$claim_month) |>
    collect_standard("available_months") |>
    dplyr::pull(.data$claim_month)
}

top_filter_values <- function(source_tbl, field = c("hcpcs", "billing", "servicing"), n = 500) {
  field <- match.arg(field)
  field_name <- switch(
    field,
    hcpcs = "hcpcs_code",
    billing = "billing_provider_npi",
    servicing = "servicing_provider_npi"
  )

  with_standard_fields(source_tbl) |>
    dplyr::group_by(value = .data[[field_name]]) |>
    dplyr::summarise(total_paid = sum(.data$TOTAL_PAID, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total_paid)) |>
    limit_result(n, default = 500L) |>
    collect_standard(paste0("top_filter_values_", field)) |>
    dplyr::pull(.data$value)
}

format_query_table <- function(data) {
  money_columns <- intersect(
    c("total_paid", "paid_per_patient", "paid_per_line"),
    names(data)
  )
  count_columns <- intersect(
    c(
      "row_count", "total_patients", "total_claim_lines", "claim_lines",
      "billing_provider_count", "servicing_provider_count", "hcpcs_count",
      "active_months"
    ),
    names(data)
  )
  decimal_columns <- intersect("lines_per_patient", names(data))

  for (col in money_columns) {
    data[[col]] <- format_dollars(data[[col]])
  }

  for (col in count_columns) {
    data[[col]] <- format_count(data[[col]])
  }

  for (col in decimal_columns) {
    data[[col]] <- format_decimal(data[[col]])
  }

  data
}

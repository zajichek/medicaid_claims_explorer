format_count <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_dollars <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
  )
}

format_decimal <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    NA_character_,
    format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits)
  )
}

format_month_label <- function(x) {
  as.character(x)
}

value_or_dash <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    "-"
  } else {
    x
  }
}

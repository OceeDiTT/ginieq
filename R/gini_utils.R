#' Gini coefficient (internal)
#'
#' @param x Numeric vector.
#' @return Gini coefficient in \[0, 1], or `NA` if empty.
#' @keywords internal
.gini <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n == 0) return(NA_real_)
  if (all(x == 0)) return(0)

  x <- sort(x)
  (2 * sum(seq_len(n) * x)) / (n * sum(x)) - (n + 1) / n
}

#' Categorise a Gini coefficient (internal)
#'
#' @param g Numeric vector of Gini coefficients on the 0-100 scale.
#' @return An ordered factor of inequality categories.
#' @keywords internal
.gini_category <- function(g) {
  cut(
    g,
    breaks = c(0, 30, 45, 60, Inf),
    labels = c("Low", "Moderate", "High", "Very High"),
    include.lowest = TRUE
  )
}

#' Detect yearly mean NTL columns (internal)
#'
#' @param nms Character vector of column names.
#' @return Character vector of matched column names (ordered by year).
#' @keywords internal
.ntl_year_cols <- function(nms) {
  cols <- grep("^NTL_Yearly_Mean_Y\\d{4}$", nms, value = TRUE)
  cols[order(sub("^NTL_Yearly_Mean_Y", "", cols))]  # chronological
}

#' Yearly-mean NTL column names, ordered chronologically (internal)
#' @keywords internal
.ntl_year_cols <- function(nms) {
  cols <- grep("^NTL_Yearly_Mean_Y\\d{4}$", nms, value = TRUE)
  cols[order(as.integer(sub("^NTL_Yearly_Mean_Y", "", cols)))]
}

#' Numeric years from yearly-mean column names (internal)
#' @keywords internal
.years_from_cols <- function(cols) {
  as.integer(sub("^NTL_Yearly_Mean_Y", "", cols))
}
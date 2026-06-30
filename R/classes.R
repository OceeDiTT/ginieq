# -------------------------------------------------------------------------
# Internal constructor
# -------------------------------------------------------------------------

new_gini_data <- function(cleaned_sf) {

  if (!inherits(cleaned_sf, "sf")) {
    stop("`cleaned_sf` must be an sf object.", call. = FALSE)
  }

  ntl_cols <- c(
    "NTL_Jan_Mean", "NTL_Feb_Mean", "NTL_Mar_Mean",
    "NTL_Apr_Mean", "NTL_May_Mean", "NTL_Jun_Mean",
    "NTL_Jul_Mean", "NTL_Aug_Mean", "NTL_Sep_Mean",
    "NTL_Oct_Mean", "NTL_Nov_Mean", "NTL_Dec_Mean"
  )

  required_cols <- c(
    ntl_cols,
    "shapeName",
    "pop",
    "Gini_Coefficient",
    "Gini_Category"
  )

  missing_cols <- setdiff(required_cols, names(cleaned_sf))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  attr(cleaned_sf, "ntl_cols") <- ntl_cols

  class(cleaned_sf) <- unique(c("gini_data", class(cleaned_sf)))

  cleaned_sf
}

# -------------------------------------------------------------------------
# User constructor
# -------------------------------------------------------------------------

#' Create a gini_data object
#'
#' Creates an S3 object of class `gini_data` from an
#' spatial `sf` object.
#'
#' @param cleaned_sf An `sf` object containing monthly nighttime lights,
#' population and Gini information.
#'
#' @return An object of class `gini_data`.
#'
#' @export
gini_data <- function(cleaned_sf) {
  new_gini_data(cleaned_sf)
}

# -------------------------------------------------------------------------
# Print method
# -------------------------------------------------------------------------

#' Print an gini_data object
#'
#' Prints a concise summary of an `gini_data` object.
#'
#' @param x An object of class `gini_data`.
#' @param ... Reserved for future use.
#'
#' @return The input object invisibly.
#'
#' @export
#' @method print gini_data
print.gini_data <- function(x, ...) {

  ntl_cols <- attr(x, "ntl_cols")

  cat("\n")
  cat("Gini Spatial Data Object\n")
  cat("-----------------------\n")
  cat("Features :", nrow(x), "\n")
  cat("Geometry :", unique(sf::st_geometry_type(x)), "\n")
  cat("CRS      :", sf::st_crs(x)$input, "\n\n")

  month_means <- sapply(
    ntl_cols,
    function(col) mean(x[[col]], na.rm = TRUE)
  )

  names(month_means) <- month.abb

  cat("Average Monthly Nighttime Lights\n")
  print(round(month_means, 3))

  cat("\nPopulation\n")
  cat(format(sum(x$pop, na.rm = TRUE), big.mark = ","), "\n")

  cat("\nMean Gini coefficient\n")
  cat(round(mean(x$Gini_Coefficient, na.rm = TRUE), 3), "\n")

  invisible(x)
}

# -------------------------------------------------------------------------
# Summary method
# -------------------------------------------------------------------------

#' Summarize an gini_data object
#'
#' Computes descriptive statistics for an `gini_data` object.
#'
#' @param object An object of class `gini_data`.
#' @param ... Reserved for future use.
#'
#' @return An object of class `summary.gini_data`.
#'
#' @export
#' @method summary gini_data
summary.gini_data <- function(object, ...) {

  ntl_cols <- attr(object, "ntl_cols")

  df <- sf::st_drop_geometry(object)

  ntl_stats <- data.frame(
    Month = month.abb,
    Min = sapply(ntl_cols, function(i) min(df[[i]], na.rm = TRUE)),
    Mean = sapply(ntl_cols, function(i) mean(df[[i]], na.rm = TRUE)),
    Median = sapply(ntl_cols, function(i) stats::median(df[[i]], na.rm = TRUE)),
    Max = sapply(ntl_cols, function(i) max(df[[i]], na.rm = TRUE)),
    SD = sapply(ntl_cols, function(i) stats::sd(df[[i]], na.rm = TRUE))
  )

  ntl_stats[-1] <- lapply(
    ntl_stats[-1],
    round,
    digits = 3
  )

  annual_ntl <- rowMeans(df[ntl_cols], na.rm = TRUE)

  out <- list(
    n_features = nrow(object),
    ntl_statistics = ntl_stats,
    annual_ntl = summary(annual_ntl),
    population = summary(df$pop),
    gini = summary(df$Gini_Coefficient),
    categories = table(df$Gini_Category)
  )

  class(out) <- "summary.gini_data"

  out
}

# -------------------------------------------------------------------------
# Print summary
# -------------------------------------------------------------------------

#' Print a summary.gini_data object
#'
#' Prints summary statistics returned by
#' [summary()].
#'
#' @param x A `summary.gini_data` object.
#' @param ... Reserved for future use.
#'
#' @return The input object invisibly.
#'
#' @export
#' @method print summary.gini_data
print.summary.gini_data <- function(x, ...) {

  cat("\nSummary of Gini Spatial Data\n")
  cat("---------------------------\n\n")

  cat("Number of features\n")
  print(x$n_features)

  cat("\nMonthly statistics\n")
  print(x$ntl_statistics)

  cat("\nAnnual nighttime lights\n")
  print(x$annual_ntl)

  cat("\nPopulation\n")
  print(x$population)

  cat("\nGini coefficient\n")
  print(x$gini)

  cat("\nGini categories\n")
  print(x$categories)

  invisible(x)
}

# -------------------------------------------------------------------------
# Plot method
# -------------------------------------------------------------------------

#' Plot an gini_data object
#'
#' Produces a choropleth map from an `gini_data`
#' object.
#'
#' @param x An object of class `gini_data`.
#' @param var Variable to display.
#' One of
#' `"ntl_annual"`,
#' `"pop"`,
#' `"Gini_Coefficient"`,
#' `"Gini_Category"`,
#' or one of the monthly nighttime lights variables.
#' @param ... Additional arguments passed to
#' [ggplot2::geom_sf()].
#'
#' @return A ggplot object.
#'
#' @export
#' @method plot gini_data
plot.gini_data <- function(x,
                          var = "ntl_annual",
                          ...) {

  ntl_cols <- attr(x, "ntl_cols")

  plot_sf <- x
  class(plot_sf) <- setdiff(class(plot_sf), "gini_data")

  if (var == "ntl_annual") {

    plot_sf$.plot <- rowMeans(
      sf::st_drop_geometry(x)[ntl_cols],
      na.rm = TRUE
    )

    title <- "Annual Mean Nighttime Lights"

  } else if (var %in% ntl_cols) {

    plot_sf$.plot <- plot_sf[[var]]

    title <- var

  } else if (var %in% c(
    "pop",
    "Gini_Coefficient",
    "Gini_Category"
  )) {

    plot_sf$.plot <- plot_sf[[var]]

    title <- var

  } else {

    stop(
      "Unknown variable: ",
      var,
      call. = FALSE
    )

  }

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.plot),
      colour = "grey40",
      linewidth = 0.1,
      ...
    ) +
    ggplot2::labs(
      title = title,
      fill = title
    ) +
    ggplot2::theme_minimal()

  if (is.numeric(plot_sf$.plot)) {

    p <- p +
      ggplot2::scale_fill_viridis_c()

  } else {

    p <- p +
      ggplot2::scale_fill_viridis_d()

  }

  p
}
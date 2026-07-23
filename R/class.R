# ginieq S3 class

# This file defines an S3 class `gini_data` built on top of an `sf` object.
# It provides constructors, and print / summary / plot methods for working
# with monthly nighttime lights (NTL), population and Gini information.


# -------------------------------------------------------------------------
# Internal constructor
# -------------------------------------------------------------------------
# Validates the input and attaches the class and
# NTL column metadata. Not intended to be called directly by users.

new_gini_data <- function(cleaned_sf) {

  # The object must be a spatial `sf` object.
  if (!inherits(cleaned_sf, "sf")) {
    stop("`cleaned_sf` must be an sf object.", call. = FALSE)
  }

  # Names of the 12 monthly nighttime lights columns expected in the data.
  ntl_cols <- c(
    "NTL_Jan_Mean", "NTL_Feb_Mean", "NTL_Mar_Mean",
    "NTL_Apr_Mean", "NTL_May_Mean", "NTL_Jun_Mean",
    "NTL_Jul_Mean", "NTL_Aug_Mean", "NTL_Sep_Mean",
    "NTL_Oct_Mean", "NTL_Nov_Mean", "NTL_Dec_Mean"
  )

  # Full set of columns that must be present for the object to be valid.
  required_cols <- c(
    ntl_cols,
    "shapeName",
    "pop",
    "Gini_Coefficient",
    "Gini_Category"
  )

  # Identify any required columns that are missing from the input
  # and fail early if any columns are missing.
  missing_cols <- setdiff(required_cols, names(cleaned_sf))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Store the NTL column names as an attribute for later use by methods.
  attr(cleaned_sf, "ntl_cols") <- ntl_cols

  # Prepend the `gini_data` class while keeping existing sf classes.
  class(cleaned_sf) <- unique(c("gini_data", class(cleaned_sf)))

  cleaned_sf
}


# -------------------------------------------------------------------------
# User constructor
# -------------------------------------------------------------------------
# Friendly, user-facing wrapper around the internal constructor.

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
# Prints a compact, human-readable overview of the object.

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
  
  # Retrieve the stored NTL column names.
  ntl_cols <- attr(x, "ntl_cols")

  # Header block with basic spatial metadata.
  cat("\n")
  cat("Gini Spatial Data Object\n")
  cat("-----------------------\n")
  cat("Features :", nrow(x), "\n")
  cat("Geometry :", unique(sf::st_geometry_type(x)), "\n")
  cat("CRS      :", sf::st_crs(x)$input, "\n\n")

  # Compute the mean nighttime lights for each month.
  month_means <- sapply(
    ntl_cols,
    function(col) mean(x[[col]], na.rm = TRUE)
  )

  # Use short month labels (Jan, Feb, ...) for display.
  names(month_means) <- month.abb

  # Print average monthly nighttime lights.
  cat("Average Monthly Nighttime Lights\n")
  print(round(month_means, 3))

  # Print total population across all features.
  cat("\nPopulation\n")
  cat(format(sum(x$pop, na.rm = TRUE), big.mark = ","), "\n")

  # Print the mean Gini coefficient.
  cat("\nMean Gini coefficient\n")
  cat(round(mean(x$Gini_Coefficient, na.rm = TRUE), 3), "\n")

  invisible(x)
}


# -------------------------------------------------------------------------
# Summary method
# -------------------------------------------------------------------------
# Computes descriptive statistics and returns them as a structured object.

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

  # Retrieve the stored NTL column names and drop geometry.
  ntl_cols <- attr(object, "ntl_cols")

  df <- sf::st_drop_geometry(object)

  # Build a per-month table of summary statistics for nighttime lights.
  ntl_stats <- data.frame(
    Month = month.abb,
    Min = sapply(ntl_cols, function(i) min(df[[i]], na.rm = TRUE)),
    Mean = sapply(ntl_cols, function(i) mean(df[[i]], na.rm = TRUE)),
    Median = sapply(ntl_cols, function(i) stats::median(df[[i]], na.rm = TRUE)),
    Max = sapply(ntl_cols, function(i) max(df[[i]], na.rm = TRUE)),
    SD = sapply(ntl_cols, function(i) stats::sd(df[[i]], na.rm = TRUE))
  )

  # Round all numeric statistic columns (everything except Month).
  ntl_stats[-1] <- lapply(
    ntl_stats[-1],
    round,
    digits = 3
  )

  # Compute an annual mean NTL value per feature (row-wise mean of months).
  annual_ntl <- rowMeans(df[ntl_cols], na.rm = TRUE)

  # Assemble the summary components into a list.
  out <- list(
    n_features = nrow(object),
    ntl_statistics = ntl_stats,
    annual_ntl = summary(annual_ntl),
    population = summary(df$pop),
    gini = summary(df$Gini_Coefficient),
    categories = table(df$Gini_Category)
  )

  # Tag with a class so it prints via print.summary.gini_data().
  class(out) <- "summary.gini_data"

  out
}


# -------------------------------------------------------------------------
# Print summary
# -------------------------------------------------------------------------
# Nicely formats the object returned by summary.gini_data().

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

  # Number of spatial features.
  cat("Number of features\n")
  print(x$n_features)

  # Per-month nighttime lights statistics.
  cat("\nMonthly statistics\n")
  print(x$ntl_statistics)

  # Distribution of annual nighttime lights.
  cat("\nAnnual nighttime lights\n")
  print(x$annual_ntl)

  # Population distribution.
  cat("\nPopulation\n")
  print(x$population)

  # Gini coefficient distribution.
  cat("\nGini coefficient\n")
  print(x$gini)

  # Counts of each Gini category.
  cat("\nGini categories\n")
  print(x$categories)

  invisible(x)
}


# -------------------------------------------------------------------------
# Plot method
# -------------------------------------------------------------------------
# Produces different visualisations depending on `var`:
#   * A choropleth map for spatial/numeric variables.
#   * A pie chart of Gini category percentages ("gini_pie").
#   * A horizontal bar chart of Gini category counts ("gini_bar").

#' Plot an gini_data object
#'
#' Produces a choropleth map, pie chart and horizontal bar chart from an `gini_data`
#' object.
#'
#' @param x An object of class `gini_data`.
#' @param var Variable to display.
#' One of
#' `"ntl_annual"`,
#' `"pop"`,
#' `"Gini_Coefficient"`,
#' `"Gini_Category"`,
#' `"gini_pie"` (pie chart of category percentages),
#' `"gini_bar"` (horizontal bar chart of category counts),
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

  # Retrieve the stored NTL column names.
  ntl_cols <- attr(x, "ntl_cols")

  # -----------------------------------------------------------------------
  # Non-spatial category charts (pie chart and horizontal bar chart)
  # -----------------------------------------------------------------------
  # These are handled first and return early because they do not use the
  # spatial geometry, only the Gini_Category counts.

  if (var %in% c("gini_pie", "gini_bar")) {

    # Drop geometry and build a data frame of category counts.
    cat_df <- sf::st_drop_geometry(x)

    # Count how many features fall into each Gini category.
    counts <- as.data.frame(
      table(Gini_Category = cat_df$Gini_Category),
      stringsAsFactors = FALSE
    )
    names(counts) <- c("Gini_Category", "Count")

    # Compute the percentage share of each category.
    counts$Percentage <- 100 * counts$Count / sum(counts$Count)

    if (var == "gini_pie") {
      # ------------------------------------------------------------------
      # Pie chart of category percentages
      # ------------------------------------------------------------------
      # A pie chart is a stacked bar in polar coordinates. I build a
      # single stacked bar (x = "") then wrap it around a circle.

      # Text labels showing the percentage inside each slice.
      counts$Label <- paste0(round(counts$Percentage, 1), "%")

      p <- ggplot2::ggplot(
        counts,
        ggplot2::aes(
          x = "",
          y = .data$Percentage,
          fill = .data$Gini_Category
        )
      ) +    
        ggplot2::geom_col(width = 1, colour = "white") +
        # Convert the stacked bar into a pie chart.
        ggplot2::coord_polar(theta = "y") +
        # Place percentage labels at the middle of each slice.
        ggplot2::geom_text(
          ggplot2::aes(label = .data$Label),
          position = ggplot2::position_stack(vjust = 0.5),
          colour = "white",
          size = 4
        ) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::labs(
          title = "Gini Category Share (%)",
          fill = "Gini Category"
        ) +
        ggplot2::theme_void()

      return(p)
    }


    if (var == "gini_bar") {

      # ------------------------------------------------------------------
      # Horizontal chart of category percentages
      # ------------------------------------------------------------------

      p <- ggplot2::ggplot(
        counts,
        ggplot2::aes(
          x = .data$Count,
          # Reorder categories by count for a cleaner chart.
          y = stats::reorder(.data$Gini_Category, .data$Count),
          fill = .data$Gini_Category
        )
      ) +
        ggplot2::geom_col(colour = "grey40") +
        # Add the count value at the end of each bar.
        ggplot2::geom_text(
          ggplot2::aes(label = .data$Count),
          hjust = -0.2,
          size = 4
        ) +
        ggplot2::scale_fill_viridis_d() +
        # Give a little extra room on the right for the labels.
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.1))
        ) +
        ggplot2::labs(
          title = "Gini Category Counts",
          x = "Count",
          y = "Gini Category",
          fill = "Gini Category"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")

      return(p)
    }
  }

  # -----------------------------------------------------------------------
  # Spatial choropleth maps
  # -----------------------------------------------------------------------

  # Work on a copy and strip the `gini_data` class so ggplot2 treats it as
  # a plain sf object.

  plot_sf <- x
  class(plot_sf) <- setdiff(class(plot_sf), "gini_data")

  if (var == "ntl_annual") {

    # Annual mean NTL: row-wise mean of the 12 monthly columns.
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

    # Population, Gini coefficient, or Gini category (as a map).
    plot_sf$.plot <- plot_sf[[var]]

    title <- var

  } else {

    # Any other value is not supported.
    stop(
      "Unknown variable: ",
      var,
      call. = FALSE
    )

  }

  # Base choropleth map.
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

  # Choose a continuous or discrete colour scale depending on the variable.
  if (is.numeric(plot_sf$.plot)) {

    p <- p +
      ggplot2::scale_fill_viridis_c()

  } else {

    p <- p +
      ggplot2::scale_fill_viridis_d()

  }

  p
}
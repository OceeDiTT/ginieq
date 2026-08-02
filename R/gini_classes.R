# ginieq S3 classes
# -----------------------------------------------------------------------------
# Classes:
#   ntl_merged    : merged yearly data.frame from aggregate_ntl_data()
#   gini_data     : cleaned sf (yearly NTL + pop) from prepare_ntl_data()
#   spatial_gini  : per-year Gini table from compute_spatial_gini()
#   temporal_gini : per-region Gini sf from compute_temporal_gini()
# -----------------------------------------------------------------------------


# =============================================================================
# ntl_merged  (requirement 1: print merged data as a table, hide .geo)
# =============================================================================

#' Create an ntl_merged object
#' @param x A merged yearly-aggregated data.frame (may contain a `.geo` column).
#' @return An object of class `ntl_merged`.
#' @export
ntl_merged <- function(x) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame.", call. = FALSE)
  class(x) <- unique(c("ntl_merged", class(x)))
  x
}

#' Print an ntl_merged object
#' @param x An `ntl_merged` object.
#' @param n Rows to display.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
#' @method print ntl_merged
print.ntl_merged <- function(x, n = 10, ...) {
  df <- x
  class(df) <- setdiff(class(df), "ntl_merged")
  if (".geo" %in% names(df)) df[[".geo"]] <- NULL

  cat("<ntl_merged>:", nrow(df), "regions x", ncol(df),
      "columns (.geo hidden)\n\n")
  print(utils::head(df, n))
  if (nrow(df) > n) cat("# ... with", nrow(df) - n, "more rows\n")
  invisible(x)
}


# =============================================================================
# gini_data  (cleaned sf with yearly NTL + population)
# =============================================================================

#' Create a gini_data object
#'
#' Wraps a cleaned `sf` object containing yearly mean NTL columns
#' (`NTL_Yearly_Mean_Y####`) and a population column.
#'
#' @param cleaned_sf An `sf` object.
#' @param pop_col Population column name (default `"pop"`).
#' @return An object of class `gini_data`.
#' @export
gini_data <- function(cleaned_sf, pop_col = "pop") {
  if (!inherits(cleaned_sf, "sf")) {
    stop("`cleaned_sf` must be an sf object.", call. = FALSE)
  }

  ntl_cols <- .ntl_year_cols(names(cleaned_sf))
  if (length(ntl_cols) == 0) {
    stop("No yearly mean NTL columns (NTL_Yearly_Mean_Y####) found.",
         call. = FALSE)
  }
  if (!(pop_col %in% names(cleaned_sf))) {
    stop("Population column '", pop_col, "' not found.", call. = FALSE)
  }

  attr(cleaned_sf, "ntl_cols") <- ntl_cols
  attr(cleaned_sf, "years")    <- .years_from_cols(ntl_cols)
  attr(cleaned_sf, "pop_col")  <- pop_col
  class(cleaned_sf) <- unique(c("gini_data", class(cleaned_sf)))
  cleaned_sf
}

#' Print a gini_data object (requirement 2: table without geometry/.geo)
#' @param x A `gini_data` object.
#' @param n Rows to display.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
#' @method print gini_data
print.gini_data <- function(x, n = 10, ...) {
  df <- sf::st_drop_geometry(x)
  if (".geo" %in% names(df)) df[[".geo"]] <- NULL

  cat("<gini_data>:", nrow(df), "regions |",
      "years:", paste(attr(x, "years"), collapse = ", "), "\n")
  cat("CRS:", sf::st_crs(x)$input, "\n\n")
  print(utils::head(df, n))
  if (nrow(df) > n) cat("# ... with", nrow(df) - n, "more rows\n")
  invisible(x)
}

#' Summarize a gini_data object
#' @param object A `gini_data` object.
#' @param ... Unused.
#' @return A `summary.gini_data` object.
#' @export
#' @method summary gini_data
summary.gini_data <- function(object, ...) {
  ntl_cols <- attr(object, "ntl_cols")
  years    <- attr(object, "years")
  pop_col  <- attr(object, "pop_col")
  df <- sf::st_drop_geometry(object)

  ntl_stats <- data.frame(
    Year   = years,
    Min    = sapply(ntl_cols, function(i) min(df[[i]], na.rm = TRUE)),
    Mean   = sapply(ntl_cols, function(i) mean(df[[i]], na.rm = TRUE)),
    Median = sapply(ntl_cols, function(i) stats::median(df[[i]], na.rm = TRUE)),
    Max    = sapply(ntl_cols, function(i) max(df[[i]], na.rm = TRUE)),
    SD     = sapply(ntl_cols, function(i) stats::sd(df[[i]], na.rm = TRUE)),
    row.names = NULL
  )
  ntl_stats[-1] <- lapply(ntl_stats[-1], round, digits = 3)

  out <- list(
    n_features    = nrow(object),
    ntl_statistics = ntl_stats,
    population    = summary(df[[pop_col]])
  )
  class(out) <- "summary.gini_data"
  out
}

#' Print a summary.gini_data object
#' @param x A `summary.gini_data` object.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
#' @method print summary.gini_data
print.summary.gini_data <- function(x, ...) {
  cat("\nSummary of gini_data\n--------------------\n")
  cat("\nNumber of features:", x$n_features, "\n")
  cat("\nYearly NTL statistics\n")
  print(x$ntl_statistics)
  cat("\nPopulation\n")
  print(x$population)
  invisible(x)
}


# ---- internal plot helpers --------------------------------------------------

# Long-format sf of (log-transformed) yearly NTL, for faceting.
.build_long_ntl <- function(x, ntl_cols, years, transform = log1p) {
  geom <- sf::st_geometry(x)
  parts <- lapply(seq_along(ntl_cols), function(i) {
    v <- transform(as.numeric(x[[ntl_cols[i]]]))
    v[!is.finite(v)] <- NA_real_
    sf::st_sf(year = factor(years[i]), value = v, geometry = geom)
  })
  do.call(rbind, parts)
}

# Long-format sf of log2 change ratios relative to the earliest (baseline) year.
.build_change_long <- function(x, ntl_cols, years) {
  geom      <- sf::st_geometry(x)
  base_i    <- which.min(years)
  base_year <- years[base_i]
  base_val  <- as.numeric(x[[ntl_cols[base_i]]])
  others    <- setdiff(seq_along(years), base_i)

  parts <- lapply(others, function(i) {
    # log2(other / baseline): positive = increase since baseline year
    v <- log2(as.numeric(x[[ntl_cols[i]]]) / base_val)
    v[!is.finite(v)] <- NA_real_
    sf::st_sf(
      comparison = factor(paste0(base_year, " \u2192 ", years[i])),
      value      = v,
      geometry   = geom
    )
  })
  do.call(rbind, parts)
}


#' Plot a gini_data object
#'
#' @param x A `gini_data` object.
#' @param var One of `"ntl_yearly"` (faceted log NTL maps per year),
#'   `"pop"` (log population map), or `"ntl_change"` (diverging change maps).
#' @param ... Passed to [ggplot2::geom_sf()].
#' @return A ggplot object.
#' @export
#' @method plot gini_data
plot.gini_data <- function(x, var = "ntl_yearly", ...) {

  ntl_cols <- attr(x, "ntl_cols")
  years    <- attr(x, "years")
  pop_col  <- attr(x, "pop_col")

  # ---- requirement 3: yearly log NTL maps, one frame, shared palette --------
  if (var == "ntl_yearly") {
    long_sf <- .build_long_ntl(x, ntl_cols, years, transform = log1p)

    return(
      ggplot2::ggplot(long_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data$value),
                         colour = "grey40", linewidth = 0.05, ...) +
        ggplot2::facet_wrap(~ .data$year) +
        ggplot2::scale_fill_viridis_c(option = "inferno") +
        ggplot2::labs(
          title = "Log Mean Nighttime Lights by Year",
          fill  = "log(NTL)"
        ) +
        ggplot2::theme_minimal()
    )
  }

  # ---- requirement 4: log population map ------------------------------------
  if (var == "pop") {
    plot_sf <- x
    class(plot_sf) <- setdiff(class(plot_sf), "gini_data")
    plot_sf$.plot <- log1p(as.numeric(x[[pop_col]]))

    return(
      ggplot2::ggplot(plot_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data$.plot),
                         colour = "grey40", linewidth = 0.1, ...) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(title = "Log Population Distribution",
                      fill = "log(pop)") +
        ggplot2::theme_minimal()
    )
  }

  # ---- requirement 5: diverging change maps, colorblind-safe ----------------
  if (var == "ntl_change") {
    change_sf <- .build_change_long(x, ntl_cols, years)

    return(
      ggplot2::ggplot(change_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data$value),
                         colour = "grey40", linewidth = 0.05, ...) +
        ggplot2::facet_wrap(~ .data$comparison) +
        # RdBu endpoints (ColorBrewer) are colour-blind safe
        ggplot2::scale_fill_gradient2(
          low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
          midpoint = 0, na.value = "grey90"
        ) +
        ggplot2::labs(
          title = "Change in Mean NTL Relative to Baseline Year",
          subtitle = "log2 ratio; positive = increase since baseline",
          fill = "log2 change"
        ) +
        ggplot2::theme_minimal()
    )
  }

  stop("Unknown `var`: ", var, call. = FALSE)
}


# =============================================================================
# spatial_gini  (per-year Gini table)
# =============================================================================

#' Create a spatial_gini object
#' @param x A data.frame from [compute_spatial_gini()].
#' @return An object of class `spatial_gini`.
#' @export
spatial_gini <- function(x) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame.", call. = FALSE)
  class(x) <- unique(c("spatial_gini", class(x)))
  x
}

#' Print a spatial_gini object (requirement 6: neat table)
#' @param x A `spatial_gini` object.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
#' @method print spatial_gini
print.spatial_gini <- function(x, ...) {
  df <- x
  class(df) <- setdiff(class(df), "spatial_gini")
  cat("<spatial_gini>: yearly spatial inequality\n\n")
  print(df, row.names = FALSE)
  invisible(x)
}

#' Plot a spatial_gini object
#'
#' @param x A `spatial_gini` object.
#' @param type `"trend"` (Gini over time with mean line, requirement 6) or
#'   `"scatter"` (mean NTL vs Gini, requirement 8).
#' @param ... Unused.
#' @return A ggplot object.
#' @export
#' @method plot spatial_gini
plot.spatial_gini <- function(x, type = "trend", ...) {

  df       <- as.data.frame(x)
  per_year <- df[df$Year != "All_Years_Mean", , drop = FALSE]
  mean_row <- df[df$Year == "All_Years_Mean", , drop = FALSE]
  per_year$YearNum <- as.integer(sub("^Y", "", per_year$Year))

  # ---- requirement 6: trend line with mean as dotted line -------------------
  if (type == "trend") {
    p <- ggplot2::ggplot(
      per_year,
      ggplot2::aes(x = .data$YearNum, y = .data$Gini_Coefficient)
    ) +
      ggplot2::geom_line(colour = "steelblue", linewidth = 1) +
      ggplot2::geom_point(size = 3, colour = "steelblue") +
      ggplot2::scale_x_continuous(breaks = per_year$YearNum) +
      ggplot2::labs(
        title = "Spatial Gini Index Over Time",
        x = "Year", y = "Spatial Gini Index (%)"
      ) +
      ggplot2::theme_minimal()

    if (nrow(mean_row) == 1) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = mean_row$Gini_Coefficient,
          linetype = "dotted", colour = "red", linewidth = 0.8
        ) +
        ggplot2::annotate(
          "text",
          x = max(per_year$YearNum),
          y = mean_row$Gini_Coefficient,
          label = paste0("Mean = ", mean_row$Gini_Coefficient, "%"),
          vjust = -0.6, hjust = 1, colour = "red", size = 3.5
        )
    }
    return(p)
  }

  # ---- requirement 8: NTL vs Gini scatter -----------------------------------
  if (type == "scatter") {
    return(
      ggplot2::ggplot(
        per_year,
        ggplot2::aes(x = .data$Mean_NTL, y = .data$Gini_Coefficient)
      ) +
        ggplot2::geom_point(size = 3, colour = "darkorange") +
        ggplot2::geom_smooth(method = "lm", se = FALSE,
                             linetype = "dashed", colour = "grey40") +
        ggplot2::geom_text(ggplot2::aes(label = .data$Year),
                           vjust = -0.8, size = 3.2) +
        ggplot2::labs(
          title = "Spatial Gini vs Average Per-Capita NTL",
          x = "Average Yearly Mean NTL (per capita)",
          y = "Spatial Gini Index (%)"
        ) +
        ggplot2::theme_minimal()
    )
  }

  stop("Unknown `type`: ", type, call. = FALSE)
}


# =============================================================================
# temporal_gini  (per-region Gini sf)
# =============================================================================

#' Create a temporal_gini object
#' @param x An `sf` object from [compute_temporal_gini()].
#' @return An object of class `temporal_gini`.
#' @export
temporal_gini <- function(x) {
  if (!inherits(x, "sf")) stop("`x` must be an sf object.", call. = FALSE)
  req <- c("Gini_Coefficient", "Gini_Category")
  miss <- setdiff(req, names(x))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  class(x) <- unique(c("temporal_gini", class(x)))
  x
}

#' Print a temporal_gini object (table without geometry)
#' @param x A `temporal_gini` object.
#' @param n Rows to display.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
#' @method print temporal_gini
print.temporal_gini <- function(x, n = 10, ...) {
  df <- sf::st_drop_geometry(x)
  cat("<temporal_gini>:", nrow(df), "regions\n")
  cat("Mean temporal Gini:",
      round(mean(df$Gini_Coefficient, na.rm = TRUE), 2), "%\n\n")
  print(utils::head(df, n))
  if (nrow(df) > n) cat("# ... with", nrow(df) - n, "more rows\n")
  invisible(x)
}

#' Plot a temporal_gini object
#'
#' @param x A `temporal_gini` object.
#' @param type `"category"` (pie + bar in one frame, requirement 7) or
#'   `"map"` (map of temporal Gini per region, requirement 7).
#' @param ... Passed to [ggplot2::geom_sf()] for `type = "map"`.
#' @return A ggplot / patchwork object.
#' @export
#' @method plot temporal_gini
plot.temporal_gini <- function(x, type = "category", ...) {

  # ---- requirement 7a: pie + bar in one figure frame ------------------------
  if (type == "category") {
    cat_df <- sf::st_drop_geometry(x)
    counts <- as.data.frame(
      table(Gini_Category = cat_df$Gini_Category),
      stringsAsFactors = FALSE
    )
    names(counts) <- c("Gini_Category", "Count")
    counts$Percentage <- 100 * counts$Count / sum(counts$Count)
    counts$Label <- paste0(round(counts$Percentage, 1), "%")

    p_pie <- ggplot2::ggplot(
      counts,
      ggplot2::aes(x = "", y = .data$Percentage, fill = .data$Gini_Category)
    ) +
      ggplot2::geom_col(width = 1, colour = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::geom_text(ggplot2::aes(label = .data$Label),
                         position = ggplot2::position_stack(vjust = 0.5),
                         colour = "white", size = 3.5) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::labs(title = "Gini Category Share (%)", fill = "Category") +
      ggplot2::theme_void()

    p_bar <- ggplot2::ggplot(
      counts,
      ggplot2::aes(x = .data$Count,
                   y = stats::reorder(.data$Gini_Category, .data$Count),
                   fill = .data$Gini_Category)
    ) +
      ggplot2::geom_col(colour = "grey40") +
      ggplot2::geom_text(ggplot2::aes(label = .data$Count),
                         hjust = -0.2, size = 3.5) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(title = "Gini Category Counts",
                    x = "Count", y = "Gini Category") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

    return(
      patchwork::wrap_plots(p_pie, p_bar, ncol = 2) +
        patchwork::plot_annotation(title = "Temporal Gini Categories")
    )
  }

  # ---- requirement 7b: map of temporal Gini per region ----------------------
  if (type == "map") {
    plot_sf <- x
    class(plot_sf) <- setdiff(class(plot_sf), "temporal_gini")

    return(
      ggplot2::ggplot(plot_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data$Gini_Coefficient),
                         colour = "grey40", linewidth = 0.1, ...) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(title = "Temporal Gini Index by Region",
                      fill = "Gini (%)") +
        ggplot2::theme_minimal()
    )
  }

  stop("Unknown `type`: ", type, call. = FALSE)
}
#' Compute spatial Gini index (one value per year)
#'
#' Computes a Gini coefficient **across regions** for each year, using
#' per-capita nighttime light (yearly mean NTL divided by population). Returns
#' a tidy table of the Gini index and category per year, plus a final row
#' giving the mean Gini across all years. Optionally exports the table as CSV.
#'
#' @param cleaned_sf An `sf` object containing yearly mean NTL columns
#'   (`NTL_Yearly_Mean_Y####`) and a population column.
#' @param output_dir Optional output directory. If `NULL`, no file is written.
#' @param file_name File name prefix for the exported CSV.
#' @param pop_col Name of the population column (default `"pop"`).
#'
#' @return A data.frame with columns `Year`, `Gini_Coefficient`, `Gini_Category`.
#' @export
#'
#' @examples
#' \dontrun{
#' spatial_gini <- compute_spatial_gini(clean_ntl_sf, tempdir())
#' }
compute_spatial_gini <- function(cleaned_sf,
                                 output_dir = NULL,
                                 file_name = "spatial_gini",
                                 pop_col = "pop") {

  # Validate input
  if (!inherits(cleaned_sf, "sf")) {
    stop("Input must be an 'sf' object.", call. = FALSE)
  }
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    stop("The output directory does not exist.", call. = FALSE)
  }

  # Drop geometry; work on the attribute table
  df <- sf::st_drop_geometry(cleaned_sf)

  # Detect yearly mean NTL columns
  ntl_cols <- .ntl_year_cols(names(df))
  if (length(ntl_cols) == 0) {
    stop("No yearly mean NTL columns (NTL_Yearly_Mean_Y####) found.",
         call. = FALSE)
  }
  if (!(pop_col %in% names(df))) {
    stop("Population column '", pop_col, "' not found.", call. = FALSE)
  }

  # Ensure numeric
  df[ntl_cols] <- lapply(df[ntl_cols], as.numeric)
  df[[pop_col]] <- as.numeric(df[[pop_col]])

  # For each year: per-capita NTL, then Gini across regions (downwards)
  gini_by_year <- vapply(
    ntl_cols,
    function(col) {
      per_capita <- df[[col]] / df[[pop_col]]
      .gini(per_capita) * 100
    },
    numeric(1)
  )
  gini_by_year <- round(gini_by_year, 1)

  # Average yearly mean NTL across regions (one value per year) - for scatter
  mean_ntl_by_year <- vapply(
    ntl_cols,
    function(col) mean(df[[col]] / df[[pop_col]], na.rm = TRUE),
    numeric(1)
  )

  # Extract clean year labels (e.g. "Y2019")
  years <- sub("^NTL_Yearly_Mean_", "", ntl_cols)

  # Build tidy per-year table
  result <- data.frame(
    Year             = years,
    Mean_NTL         = round(unname(mean_ntl_by_year), 4),
    Gini_Coefficient = unname(gini_by_year),
    stringsAsFactors = FALSE
  )
  result$Gini_Category <- .gini_category(result$Gini_Coefficient)

  # Append a mean-across-years row
  mean_gini <- round(mean(gini_by_year, na.rm = TRUE), 1)
  result <- rbind(
    result,
    data.frame(
      Year             = "All_Years_Mean",
      Mean_NTL         = round(mean(mean_ntl_by_year, na.rm = TRUE), 4),
      Gini_Coefficient = mean_gini,
      Gini_Category    = .gini_category(mean_gini),
      stringsAsFactors = FALSE
    )
  )

  message("Mean spatial Gini across all years: ", mean_gini, "%")

  # Export CSV
  if (!is.null(output_dir)) {
    utils::write.csv(
      result,
      file.path(output_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }

  spatial_gini(result)
}
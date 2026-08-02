#' Compute temporal Gini index (one value per region)
#'
#' Computes a Gini coefficient **across years** for each region (row-wise),
#' using per-capita nighttime light (yearly mean NTL divided by population).
#' The resulting Gini index and category are merged back onto the input `sf`
#' object. Optionally exports the result as CSV and GeoPackage.
#'
#' @param cleaned_sf An `sf` object containing yearly mean NTL columns
#'   (`NTL_Yearly_Mean_Y####`) and a population column.
#' @param output_dir Optional output directory. If `NULL`, no files are written.
#' @param file_name File name prefix for the exported files.
#' @param pop_col Name of the population column (default `"pop"`).
#'
#' @return An `sf` object with `Gini_Coefficient` and `Gini_Category` added.
#' @export
#'
#' @examples
#' \dontrun{
#' temporal_gini <- compute_temporal_gini(clean_ntl_sf, tempdir())
#' }
compute_temporal_gini <- function(cleaned_sf,
                                  output_dir = NULL,
                                  file_name = "temporal_gini",
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

  # Per-capita NTL for every year column
  per_capita <- df[ntl_cols] / df[[pop_col]]

  # Gini row-wise: one value per region (across years)
  gini_region <- apply(per_capita, 1, .gini) * 100
  gini_region <- round(gini_region, 1)

  # Merge results back onto the sf object
  out_sf <- cleaned_sf
  out_sf$Gini_Coefficient <- gini_region
  out_sf$Gini_Category    <- .gini_category(gini_region)

  # Export CSV (attributes only)
  if (!is.null(output_dir)) {
    utils::write.csv(
      sf::st_drop_geometry(out_sf),
      file.path(output_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )

    sf::st_write(
      out_sf,
      dsn = file.path(output_dir, paste0(file_name, ".gpkg")),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }

  temporal_gini(out_sf)
}
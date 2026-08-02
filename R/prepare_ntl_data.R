#' Load, clean, and spatialise aggregated NTL data
#'
#' Builds on `aggregate_ntl_data()`. It merges and yearly-aggregates the input
#' files, filters by a country's three-letter code, optionally writes a cleaned
#' CSV, and returns an `sf` object. Column detection/renaming is handled
#' automatically inside `aggregate_ntl_data()`.
#'
#' @param csv_files A character vector of file paths (max 5). If `NULL`, a
#'   default packaged file is used.
#' @param country_code A three-letter ISO code (e.g. `"NGA"`) used to filter the
#'   `shapeGroup` column. If `NULL`, no filtering is applied.
#' @param output_file Optional path to write the cleaned CSV (without geometry).
#' @param id_col Region identifier column passed to `aggregate_ntl_data()`.
#' @param ... Additional arguments passed to `aggregate_ntl_data()`
#'   (e.g. `labels`, `aggregate_fun`).
#'
#' @return An `sf` object of the cleaned, yearly-aggregated NTL data.
#' @export
#'
#' @examples
#' nigeria_sf <- prepare_ntl_data(country_code = "NGA")
#' nigeria_sf
prepare_ntl_data <- function(csv_files = NULL,
                             country_code = NULL,
                             output_file = NULL,
                             id_col = "shapeID",
                             ...) {

  # Step 1: aggregate & merge the input files (yearly mean + sum per file)
  data <- aggregate_ntl_data(csv_files = csv_files, id_col = id_col, ...)

  # Step 2: filter by country code if provided
  if (!is.null(country_code) && "shapeGroup" %in% names(data)) {
    data <- data |>
      dplyr::filter(.data$shapeGroup == country_code)
  }

  # Step 3: export cleaned CSV (attributes only, no geometry) if requested
  if (!is.null(output_file)) {
    csv_out <- data[, !(names(data) %in% ".geo"), drop = FALSE]
    utils::write.csv(csv_out, output_file, row.names = FALSE)
  }

  # Step 4: convert GeoJSON geometry column to an sf object
  if (!(".geo" %in% names(data))) {
    stop("No '.geo' geometry column found in the input data.")
  }

  geom <- geojsonsf::geojson_sfc(data[[".geo"]])

  cleaned_sf <- sf::st_sf(
    data |> dplyr::select(-dplyr::all_of(".geo")),
    geometry = geom,
    crs = 4326
  )

  gini_data(cleaned_sf)
}
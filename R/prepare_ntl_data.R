#' Load and prepare nighttime light data
#' 
#' This function takes an input CSV file, wrangle it, 
#' and returns a cleaned sf data ready for analysis

#' @param csv_file A Character string- file path of the input csv file
#' @export
#' 
#' @examples
#' nigeria_sf <- prepare_ntl_data()
#' nigeria_sf

prepare_ntl_data <- function(csv_file = NULL) {

  if (is.null(csv_file)) {
    csv_file <- system.file(
      "extdata",
      "Nigeria_ntl_pop.csv",
      package = "ginieq"
    )
  }

  data <- utils::read.csv(
    csv_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Rename the columns
  rename_map <- c(
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-01-01" = "NTL_Jan_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-02-01" = "NTL_Feb_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-03-01" = "NTL_Mar_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-04-01" = "NTL_Apr_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-05-01" = "NTL_May_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-06-01" = "NTL_Jun_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-07-01" = "NTL_Jul_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-08-01" = "NTL_Aug_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-09-01" = "NTL_Sep_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-10-01" = "NTL_Oct_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-11-01" = "NTL_Nov_Mean",
  "DNB_BRDF_Corrected_NTL_sum_mean_2023-12-01" = "NTL_Dec_Mean",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-01-01" = "NTL_Jan_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-02-01" = "NTL_Feb_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-03-01" = "NTL_Mar_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-04-01" = "NTL_Apr_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-05-01" = "NTL_May_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-06-01" = "NTL_Jun_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-07-01" = "NTL_Jul_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-08-01" = "NTL_Aug_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-09-01" = "NTL_Sep_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-10-01" = "NTL_Oct_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-11-01" = "NTL_Nov_Sum",
  "DNB_BRDF_Corrected_NTL_sum_sum_2023-12-01" = "NTL_Dec_Sum"
  )

  matched <- match(names(rename_map), names(data))

  names(data)[matched[!is.na(matched)]] <- unname(rename_map[!is.na(matched)])
 
  # Filter the country Nigeria
  if ("shapeGroup" %in% names(data)) {
    data <- data |>
      dplyr::filter(.data$shapeGroup == "NGA")
  }

  # Convert GeoJSON geometry column to sf geometry
  geom <- geojsonsf::geojson_sfc(data[[".geo"]])

  cleaned_sf <- sf::st_sf(
    data |> dplyr::select(-dplyr::all_of(".geo")),
    geometry = geom,
    crs = 4326
  )

  return(cleaned_sf)
}
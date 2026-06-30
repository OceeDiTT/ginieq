#' Compute Gini Index
#'
#' Computes a Gini coefficient from nighttime light data and returns an sf object
#' with added inequality metrics. Also exports CSV and GeoPackage outputs.
#'
#' @param cleaned_sf An sf object containing NTL and population data
#' @param output_dir Output directory for exported files
#' @param file_name File name prefix for outputs
#'
#' @return An sf object with Gini metrics added
#' @export
#'
#' @examples
#' \dontrun{
#' germany_gini <- compute_gini_index(clean_ntl_sf, tempdir())
#' }

compute_gini_index <- function(cleaned_sf, output_dir, file_name = "gini_index") {

    # Validate input
    if (!inherits(cleaned_sf, "sf")) {
        stop("Input must be an 'sf' object.", call. = FALSE)
    }

    # Check that the directory exists
    if (!dir.exists(output_dir)) {
        stop("The output directory does not exist.", call. = FALSE)
    }

    ntl_cols <- c(
        "NTL_Jan_Mean", "NTL_Feb_Mean", "NTL_Mar_Mean",
        "NTL_Apr_Mean", "NTL_May_Mean", "NTL_Jun_Mean",
        "NTL_Jul_Mean", "NTL_Aug_Mean", "NTL_Sep_Mean",
        "NTL_Oct_Mean", "NTL_Nov_Mean", "NTL_Dec_Mean"
    )

    # Work only on attribute table
    df <- sf::st_drop_geometry(cleaned_sf)

    # Ensure numeric safety
    df[ntl_cols] <- lapply(df[ntl_cols], as.numeric)
    df$pop <- as.numeric(df$pop)

    # Transform: log(1 + x) and per-capita scaling
    df[ntl_cols] <- lapply(df[ntl_cols], log1p)

    for (col in ntl_cols) {
        df[[col]] <- df[[col]] / df$pop
    }

    # Vectorised Gini function
    gini <- function(x) {
        x <- x[!is.na(x)]
        n <- length(x)

        if (n == 0) return(NA_real_)
        if (all(x == 0)) return(0)

        x <- sort(x)
        (2 * sum(seq_len(n) * x)) / (n * sum(x)) - (n + 1) / n
    }

    # Apply Gini row-wise
    df$Gini_Coefficient <- apply(df[ntl_cols], 1, gini) * 100
    df$Gini_Coefficient <- round(df$Gini_Coefficient, 1)

    # Gini Classification
    df$Gini_Category <- cut(
        df$Gini_Coefficient,
        breaks = c(0, 30, 45, 60, Inf),
        labels = c("Low", "Moderate", "High", "Very High"),
        include.lowest = TRUE
    )

    # Summary statistic
    message("Mean Gini Coefficient: ", round(mean(df$Gini_Coefficient, na.rm = TRUE), 2), "%")

    # Export outputs
    utils::write.csv(
        df,
        file.path(output_dir, paste0(file_name, ".csv")),
        row.names = FALSE
    )
  
    # Reattach geometry
    out_sf <- cleaned_sf
    out_sf$Gini_Coefficient <- df$Gini_Coefficient
    out_sf$Gini_Category <- df$Gini_Category

    sf::st_write(
        out_sf,
        dsn = file.path(output_dir, paste0(file_name, ".gpkg")),
        delete_dsn = TRUE,
        quiet = TRUE
    )

    

    return(gini_data(out_sf))
}
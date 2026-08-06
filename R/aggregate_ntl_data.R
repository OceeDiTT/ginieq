#' Aggregate monthly NTL data and merge multiple CSV files by region
#'
#' Takes up to 5 CSV files where each row represents a region. For each file it
#' aggregates the 12 monthly **mean** and 12 monthly **sum** NTL columns into a
#' single yearly value each, then merges all files together on the region
#' identifier. The merged, yearly-aggregated CSV is exported.
#'
#' @param csv_files A character vector of file paths (max 5). If `NULL`, the
#'   default packaged files are used.
#' @param output_file Path to write the merged yearly-aggregated CSV. If `NULL`,
#'   nothing is written.
#' @param id_col Name of the region identifier column used to join files
#'   (default `"shapeID"`).
#' @param labels Optional character vector of labels (one per file) used to
#'   suffix each file's yearly columns. Defaults to the file names.
#' @param aggregate_fun Function to collapse the 12 monthly values into one
#'   yearly value (default `mean`).
#'
#' @return A data.frame of the merged data with yearly aggregated NTL values.
#' @export
#'
#' @examples
#' merged <- aggregate_ntl_data()
#' head(merged)
aggregate_ntl_data <- function(csv_files = NULL,
                               output_file = NULL,
                               id_col = "shapeID",
                               labels = NULL,
                               aggregate_fun = mean) {

  # Default to the 5 packaged CSV files if none supplied
  if (is.null(csv_files)) {
    default_names <- c(
      "Nigeria_ntl_pop2017.csv",
      "Nigeria_ntl_pop2019.csv",
      "Nigeria_ntl_pop2021.csv",
      "Nigeria_ntl_pop2023.csv",
      "Nigeria_ntl_pop2025.csv"
    )

    # Resolve each file separately so missing files reliably return ""
    csv_files <- vapply(
      default_names,
      function(f) system.file("extdata", f, package = "ginieq"),
      character(1)
    )

    missing <- default_names[csv_files == ""]
    if (length(missing) > 0) {
      stop(
        "The following packaged files were not found in inst/extdata: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # Drop any names so they don't propagate into the loop / results
  csv_files <- unname(csv_files)

  n <- length(csv_files)
  if (n == 0) {
    stop("No CSV files supplied.")
  }
  if (n > 5) {
    stop("A maximum of 5 CSV files can be supplied.")
  }

  # Build labels (one per file); default to file names without extension
  if (is.null(labels)) {
    labels <- tools::file_path_sans_ext(basename(csv_files))
  }
  labels <- as.character(labels)
  if (length(labels) != n) {
    stop("`labels` must have the same length as `csv_files`.")
  }

  # aggregate one file's monthly means & sums into yearly values
  aggregate_one <- function(file, label, keep_metadata) {

    data <- utils::read.csv(
      file,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (!(id_col %in% names(data))) {
      stop("id_col '", id_col, "' not found in file: ", file)
    }

    mean_cols <- grep("mean_\\d{4}-\\d{2}-\\d{2}$", names(data), value = TRUE)
    sum_cols  <- grep("sum_\\d{4}-\\d{2}-\\d{2}$",  names(data), value = TRUE)

    # Auto-detect the year from the column names; fall back to `label`
    all_dated <- c(mean_cols, sum_cols)
    year_tag <- label
    if (length(all_dated) > 0) {
      years <- unique(sub(".*_(\\d{4})-\\d{2}-\\d{2}$", "\\1", all_dated))
      if (length(years) == 1) {
        year_tag <- paste0("Y", years)          # e.g. "Y2019"
      } else {
        warning("Multiple years found in ", basename(file),
                "; using label '", label, "' instead.")
      }
    }

    yearly <- data.frame(id = data[[id_col]], stringsAsFactors = FALSE)
    names(yearly) <- id_col

    if (length(mean_cols) > 0) {
      yearly[[paste0("NTL_Yearly_Mean_", year_tag)]] <-
        apply(data[mean_cols], 1, function(x) aggregate_fun(x, na.rm = TRUE))
    }
    if (length(sum_cols) > 0) {
      yearly[[paste0("NTL_Yearly_Sum_", year_tag)]] <-
        apply(data[sum_cols], 1, function(x) aggregate_fun(x, na.rm = TRUE))
    }

    if (keep_metadata) {
      monthly_cols <- c(mean_cols, sum_cols)
      metadata <- data[, !(names(data) %in% monthly_cols), drop = FALSE]
      out <- merge(metadata, yearly, by = id_col, sort = FALSE)
      return(out)
    }

    yearly
  }

  # Aggregate each file (keep full metadata + geometry only from the first).
  # Iterate over an index -> bulletproof against length mismatches.
  aggregated_list <- lapply(seq_len(n), function(i) {
    aggregate_one(
      csv_files[i],
      labels[i],
      keep_metadata = (i == 1L)
    )
  })

  # Merge all files on the region id
  merged <- Reduce(
    function(x, y) merge(x, y, by = id_col, sort = FALSE),
    aggregated_list
  )

  # Export merged CSV if requested
  if (!is.null(output_file)) {
    utils::write.csv(merged, output_file, row.names = FALSE)
  }

  ntl_merged(merged)
}
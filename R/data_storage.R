#' Save Chart Data
#'
#' Saves scraped chart data to various file formats with metadata
#'
#' @param data Chart data tibble to save
#' @param filename Base filename (without extension)
#' @param format File format: "rds", "csv", "json", or "excel" (default: "rds")
#' @param data_dir Directory to save data (default: "spotify_data")
#' @param include_metadata Logical, whether to include metadata (default: TRUE)
#' @param compress Logical, whether to compress the file (default: TRUE)
#'
#' @return Character string with full path to saved file
#' @export
#'
#' @examples
#' \dontrun{
#' # Save data in different formats
#' data <- get_daily_charts("2025-09-01")
#'
#' # Save as RDS (default, preserves R data types)
#' save_chart_data(data, "daily_charts_2025_09_01")
#'
#' # Save as CSV for external use
#' save_chart_data(data, "charts", format = "csv")
#'
#' # Save as JSON
#' save_chart_data(data, "charts", format = "json")
#' }
save_chart_data <- function(data, filename, format = "rds", data_dir = "spotify_data",
                           include_metadata = TRUE, compress = TRUE) {

  if (nrow(data) == 0) {
    warning("No data to save")
    return(NULL)
  }

  # Create data directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # Add metadata if requested
  if (include_metadata) {
    metadata <- create_data_metadata(data)
    data_to_save <- list(
      data = data,
      metadata = metadata
    )
  } else {
    data_to_save <- data
  }

  # Determine file extension and save method
  file_path <- switch(format,
    "rds" = {
      path <- file.path(data_dir, paste0(filename, ".rds"))
      if (compress) {
        saveRDS(data_to_save, path, compress = TRUE)
      } else {
        saveRDS(data_to_save, path, compress = FALSE)
      }
      path
    },
    "csv" = {
      path <- file.path(data_dir, paste0(filename, ".csv"))
      if (include_metadata) {
        # Save data only for CSV, metadata separately
        readr::write_csv(data_to_save$data, path)
        # Save metadata as separate JSON file
        metadata_path <- file.path(data_dir, paste0(filename, "_metadata.json"))
        jsonlite::write_json(data_to_save$metadata, metadata_path, pretty = TRUE)
      } else {
        readr::write_csv(data_to_save, path)
      }
      path
    },
    "json" = {
      path <- file.path(data_dir, paste0(filename, ".json"))
      jsonlite::write_json(data_to_save, path, pretty = TRUE)
      path
    },
    "excel" = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required for Excel export. Install with: install.packages('openxlsx')")
      }
      path <- file.path(data_dir, paste0(filename, ".xlsx"))
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Chart Data")
      openxlsx::writeData(wb, "Chart Data", if (include_metadata) data_to_save$data else data_to_save)

      if (include_metadata) {
        openxlsx::addWorksheet(wb, "Metadata")
        metadata_df <- data.frame(
          Property = names(data_to_save$metadata),
          Value = sapply(data_to_save$metadata, function(x) {
            if (is.list(x)) jsonlite::toJSON(x, auto_unbox = TRUE) else as.character(x)
          })
        )
        openxlsx::writeData(wb, "Metadata", metadata_df)
      }

      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      path
    },
    stop(glue::glue("Unsupported format: {format}. Use 'rds', 'csv', 'json', or 'excel'"))
  )

  message(glue::glue("Data saved: {file_path} ({format_file_size(file.size(file_path))})"))
  return(file_path)
}

#' Load Chart Data
#'
#' Loads previously saved chart data with automatic format detection
#'
#' @param filepath Full path to the data file
#' @param format Optional format specification (auto-detected if NULL)
#'
#' @return Tibble with chart data (metadata accessible via attributes if present)
#' @export
#'
#' @examples
#' \dontrun{
#' # Load RDS file (with metadata)
#' data <- load_chart_data("spotify_data/daily_charts_2025_09_01.rds")
#'
#' # Load CSV file
#' data <- load_chart_data("spotify_data/charts.csv")
#'
#' # Access metadata if available
#' metadata <- attr(data, "metadata")
#' }
load_chart_data <- function(filepath, format = NULL) {

  if (!file.exists(filepath)) {
    stop(glue::glue("File not found: {filepath}"))
  }

  # Auto-detect format if not specified
  if (is.null(format)) {
    format <- detect_file_format(filepath)
  }

  # Load data based on format
  loaded_data <- switch(format,
    "rds" = {
      data <- readRDS(filepath)
      if (is.list(data) && "data" %in% names(data) && "metadata" %in% names(data)) {
        # Data with metadata
        result <- data$data
        attr(result, "metadata") <- data$metadata
        result
      } else {
        # Plain data
        data
      }
    },
    "csv" = {
      data <- readr::read_csv(filepath, show_col_types = FALSE)
      # Try to load metadata if exists
      metadata_path <- sub("\\.csv$", "_metadata.json", filepath)
      if (file.exists(metadata_path)) {
        metadata <- jsonlite::read_json(metadata_path)
        attr(data, "metadata") <- metadata
      }
      data
    },
    "json" = {
      data <- jsonlite::read_json(filepath, simplifyVector = TRUE)
      if (is.list(data) && "data" %in% names(data)) {
        result <- tibble::as_tibble(data$data)
        if ("metadata" %in% names(data)) {
          attr(result, "metadata") <- data$metadata
        }
        result
      } else {
        tibble::as_tibble(data)
      }
    },
    "excel" = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required for Excel import. Install with: install.packages('openxlsx')")
      }
      data <- openxlsx::read.xlsx(filepath, sheet = "Chart Data")
      # Try to load metadata if exists
      tryCatch({
        metadata_df <- openxlsx::read.xlsx(filepath, sheet = "Metadata")
        if (nrow(metadata_df) > 0) {
          metadata <- setNames(as.list(metadata_df$Value), metadata_df$Property)
          attr(data, "metadata") <- metadata
        }
      }, error = function(e) {
        # Metadata sheet doesn't exist, continue without it
      })
      tibble::as_tibble(data)
    },
    stop(glue::glue("Unsupported format: {format}"))
  )

  message(glue::glue("Data loaded: {filepath} ({nrow(loaded_data)} rows)"))
  return(loaded_data)
}

#' List Saved Data Files
#'
#' Lists all saved chart data files in the data directory
#'
#' @param data_dir Directory containing data files (default: "spotify_data")
#' @param pattern Optional pattern to filter files
#' @param include_details Logical, whether to include file details (default: TRUE)
#'
#' @return Tibble with information about saved files
#' @export
#'
#' @examples
#' \dontrun{
#' # List all data files
#' files <- list_saved_data()
#'
#' # List files with specific pattern
#' recent_files <- list_saved_data(pattern = "2025")
#' }
list_saved_data <- function(data_dir = "spotify_data", pattern = NULL, include_details = TRUE) {

  if (!dir.exists(data_dir)) {
    message(glue::glue("Data directory does not exist: {data_dir}"))
    return(tibble::tibble())
  }

  # Get list of data files
  extensions <- c("rds", "csv", "json", "xlsx")
  pattern_regex <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")

  files <- list.files(data_dir, pattern = pattern_regex, full.names = TRUE)

  # Apply additional pattern filter if specified
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
  }

  if (length(files) == 0) {
    message("No data files found")
    return(tibble::tibble())
  }

  # Create file information tibble
  file_info <- tibble::tibble(
    filename = basename(files),
    filepath = files,
    format = detect_file_format(files),
    size_bytes = file.size(files),
    size_formatted = format_file_size(size_bytes),
    modified = file.mtime(files)
  )

  # Add detailed information if requested
  if (include_details) {
    file_info <- file_info %>%
      dplyr::mutate(
        age_days = as.numeric(difftime(Sys.time(), .data$modified, units = "days")),
        age_formatted = format_age(.data$age_days)
      )

    # Try to get row counts for data files (this might be slow for large files)
    tryCatch({
      file_info <- file_info %>%
        dplyr::mutate(
          estimated_rows = sapply(.data$filepath, function(f) {
            tryCatch({
              if (detect_file_format(f) == "csv") {
                # Quick row count for CSV
                lines <- readLines(f, n = 2)
                return(paste("~", length(readLines(f)) - 1, "rows"))
              } else {
                return("Unknown")
              }
            }, error = function(e) "Unknown")
          })
        )
    }, error = function(e) {
      # Skip row counting if it fails
    })
  }

  file_info <- file_info %>%
    dplyr::arrange(dplyr::desc(.data$modified))

  return(file_info)
}

#' Create Data Backup
#'
#' Creates a backup of all data files
#'
#' @param data_dir Source data directory (default: "spotify_data")
#' @param backup_dir Backup directory (default: "spotify_data_backup")
#' @param include_timestamp Logical, whether to include timestamp in backup folder name (default: TRUE)
#' @param compress Logical, whether to compress the backup (default: TRUE)
#'
#' @return Path to backup directory or file
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a backup
#' backup_path <- create_data_backup()
#'
#' # Create compressed backup
#' backup_path <- create_data_backup(compress = TRUE)
#' }
create_data_backup <- function(data_dir = "spotify_data", backup_dir = "spotify_data_backup",
                              include_timestamp = TRUE, compress = TRUE) {

  if (!dir.exists(data_dir)) {
    stop(glue::glue("Source data directory does not exist: {data_dir}"))
  }

  # Create timestamped backup directory name
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_name <- paste0(backup_dir, "_", timestamp)
  } else {
    backup_name <- backup_dir
  }

  # Copy files to backup directory
  if (dir.exists(backup_name)) {
    unlink(backup_name, recursive = TRUE)
  }

  tryCatch({
    file.copy(data_dir, dirname(backup_name), recursive = TRUE)
    file.rename(file.path(dirname(backup_name), basename(data_dir)), backup_name)

    files_backed_up <- length(list.files(backup_name, recursive = TRUE))
    message(glue::glue("Backup created: {backup_name} ({files_backed_up} files)"))

    # Compress if requested
    if (compress) {
      backup_zip <- paste0(backup_name, ".zip")
      utils::zip(backup_zip, backup_name, flags = "-r9X")
      unlink(backup_name, recursive = TRUE)
      message(glue::glue("Backup compressed: {backup_zip}"))
      return(backup_zip)
    }

    return(backup_name)

  }, error = function(e) {
    stop(glue::glue("Backup failed: {e$message}"))
  })
}

#' Restore Data from Backup
#'
#' Restores data from a backup
#'
#' @param backup_path Path to backup directory or zip file
#' @param restore_dir Directory to restore to (default: "spotify_data")
#' @param overwrite Logical, whether to overwrite existing files (default: FALSE)
#'
#' @return Logical, TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Restore from backup directory
#' restore_data_backup("spotify_data_backup_20250901_120000")
#'
#' # Restore from compressed backup
#' restore_data_backup("spotify_data_backup_20250901_120000.zip")
#' }
restore_data_backup <- function(backup_path, restore_dir = "spotify_data", overwrite = FALSE) {

  if (!file.exists(backup_path) && !dir.exists(backup_path)) {
    stop(glue::glue("Backup not found: {backup_path}"))
  }

  if (dir.exists(restore_dir) && !overwrite) {
    stop(glue::glue("Restore directory already exists: {restore_dir}. Use overwrite = TRUE to replace."))
  }

  tryCatch({
    if (dir.exists(restore_dir) && overwrite) {
      unlink(restore_dir, recursive = TRUE)
    }

    # Handle zip files
    if (grepl("\\.zip$", backup_path)) {
      temp_dir <- tempdir()
      utils::unzip(backup_path, exdir = temp_dir)
      extracted_dir <- file.path(temp_dir, tools::file_path_sans_ext(basename(backup_path)))
      file.rename(extracted_dir, restore_dir)
    } else {
      # Handle directory backups
      file.copy(backup_path, dirname(restore_dir), recursive = TRUE)
      file.rename(file.path(dirname(restore_dir), basename(backup_path)), restore_dir)
    }

    files_restored <- length(list.files(restore_dir, recursive = TRUE))
    message(glue::glue("Data restored: {restore_dir} ({files_restored} files)"))

    return(TRUE)

  }, error = function(e) {
    stop(glue::glue("Restore failed: {e$message}"))
  })
}

#' Validate Data Integrity
#'
#' Checks the integrity and consistency of saved chart data
#'
#' @param data_dir Directory containing data files (default: "spotify_data")
#' @param fix_issues Logical, whether to attempt to fix found issues (default: FALSE)
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' # Check data integrity
#' validation <- validate_data_integrity()
#'
#' # Check and fix issues
#' validation <- validate_data_integrity(fix_issues = TRUE)
#' }
validate_data_integrity <- function(data_dir = "spotify_data", fix_issues = FALSE) {

  if (!dir.exists(data_dir)) {
    return(list(
      status = "error",
      message = glue::glue("Data directory does not exist: {data_dir}")
    ))
  }

  files <- list_saved_data(data_dir, include_details = FALSE)

  if (nrow(files) == 0) {
    return(list(
      status = "warning",
      message = "No data files found",
      issues = character(0),
      fixed = character(0)
    ))
  }

  issues <- character(0)
  fixed <- character(0)

  # Check each file
  for (i in seq_len(nrow(files))) {
    file_path <- files$filepath[i]
    file_format <- files$format[i]

    tryCatch({
      # Try to load the file
      data <- load_chart_data(file_path, format = file_format)

      # Basic data validation
      if (nrow(data) == 0) {
        issues <- c(issues, glue::glue("{basename(file_path)}: Empty data file"))
      }

      # Check for required columns (if it's chart data)
      expected_cols <- c("track_name", "main_artist", "chart_position", "chart_date")
      missing_cols <- expected_cols[!expected_cols %in% names(data)]

      if (length(missing_cols) > 0) {
        issues <- c(issues, glue::glue("{basename(file_path)}: Missing columns: {paste(missing_cols, collapse = ', ')}"))
      }

      # Check for data type issues
      if ("chart_date" %in% names(data)) {
        if (!inherits(data$chart_date, "Date")) {
          issue_msg <- glue::glue("{basename(file_path)}: chart_date is not Date type")
          issues <- c(issues, issue_msg)

          if (fix_issues) {
            tryCatch({
              data$chart_date <- as.Date(data$chart_date)
              save_chart_data(data, tools::file_path_sans_ext(basename(file_path)),
                            format = file_format, data_dir = data_dir)
              fixed <- c(fixed, issue_msg)
            }, error = function(e) {
              # Could not fix
            })
          }
        }
      }

    }, error = function(e) {
      issues <- c(issues, glue::glue("{basename(file_path)}: Could not load file - {e$message}"))
    })
  }

  # Summary
  status <- if (length(issues) == 0) "ok" else if (length(issues) > length(fixed)) "warning" else "fixed"

  result <- list(
    status = status,
    files_checked = nrow(files),
    issues_found = length(issues),
    issues_fixed = length(fixed),
    issues = issues,
    fixed = fixed,
    message = glue::glue("Checked {nrow(files)} files, found {length(issues)} issues, fixed {length(fixed)}")
  )

  return(result)
}

# Internal helper functions

#' Create Data Metadata
#' @keywords internal
create_data_metadata <- function(data) {
  list(
    created_at = Sys.time(),
    package_version = utils::packageVersion("spotify"),
    data_summary = list(
      rows = nrow(data),
      columns = ncol(data),
      column_names = names(data),
      date_range = if ("chart_date" %in% names(data)) range(data$chart_date, na.rm = TRUE) else NULL,
      unique_artists = if ("main_artist" %in% names(data)) length(unique(data$main_artist)) else NULL,
      unique_tracks = if ("track_uri" %in% names(data)) length(unique(data$track_uri)) else NULL,
      regions = if ("region" %in% names(data)) unique(data$region) else NULL
    ),
    data_types = sapply(data, class),
    checksum = digest::digest(data, algo = "md5")
  )
}

#' Detect File Format
#' @keywords internal
detect_file_format <- function(filepath) {
  extension <- tolower(tools::file_ext(filepath))
  switch(extension,
    "rds" = "rds",
    "csv" = "csv",
    "json" = "json",
    "xlsx" = "excel",
    "xls" = "excel",
    "unknown"
  )
}

#' Format File Size
#' @keywords internal
format_file_size <- function(bytes) {
  if (is.na(bytes)) return("Unknown")

  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- 1

  size <- bytes
  while (size >= 1024 && unit_index < length(units)) {
    size <- size / 1024
    unit_index <- unit_index + 1
  }

  return(sprintf("%.1f %s", size, units[unit_index]))
}

#' Format Age
#' @keywords internal
format_age <- function(age_days) {
  if (is.na(age_days)) return("Unknown")

  if (age_days < 1) {
    return("< 1 day")
  } else if (age_days < 7) {
    return(glue::glue("{round(age_days)} days"))
  } else if (age_days < 30) {
    return(glue::glue("{round(age_days / 7)} weeks"))
  } else if (age_days < 365) {
    return(glue::glue("{round(age_days / 30)} months"))
  } else {
    return(glue::glue("{round(age_days / 365, 1)} years"))
  }
}
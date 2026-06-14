#' Rate Limiting for API Requests
#'
#' Implements rate limiting to be respectful to Spotify's servers
#'
#' @param delay Minimum seconds between requests (default: 1)
#' @param max_requests Maximum requests per time window (default: 60)
#' @param time_window Time window in seconds (default: 60)
#'
#' @return NULL (used for side effects)
#' @export
#'
#' @examples
#' \dontrun{
#' # Wait at least 1 second between requests
#' rate_limit(delay = 1)
#'
#' # More conservative rate limiting
#' rate_limit(delay = 2, max_requests = 30)
#' }
rate_limit <- function(delay = 1, max_requests = 60, time_window = 60) {

  # Initialize rate limiting environment if it doesn't exist
  if (!exists("spotify_rate_limit", envir = .GlobalEnv)) {
    assign("spotify_rate_limit", list(
      requests = numeric(0),
      last_request = NULL
    ), envir = .GlobalEnv)
  }

  rate_env <- get("spotify_rate_limit", envir = .GlobalEnv)

  current_time <- as.numeric(Sys.time())

  # Clean old requests outside time window
  cutoff_time <- current_time - time_window
  rate_env$requests <- rate_env$requests[rate_env$requests > cutoff_time]

  # Check if we need to wait due to request frequency
  if (length(rate_env$requests) >= max_requests) {
    wait_time <- rate_env$requests[1] + time_window - current_time
    if (wait_time > 0) {
      message(glue::glue("Rate limiting: waiting {round(wait_time, 1)} seconds..."))
      Sys.sleep(wait_time)
    }
  }

  # Check if we need to wait due to minimum delay
  if (!is.null(rate_env$last_request)) {
    time_since_last <- current_time - rate_env$last_request
    if (time_since_last < delay) {
      wait_time <- delay - time_since_last
      Sys.sleep(wait_time)
    }
  }

  # Update rate limiting tracking
  rate_env$requests <- c(rate_env$requests, as.numeric(Sys.time()))
  rate_env$last_request <- as.numeric(Sys.time())

  # Update global environment
  assign("spotify_rate_limit", rate_env, envir = .GlobalEnv)
}

#' Cache Data to Avoid Re-scraping
#'
#' Implements a simple file-based caching system for scraped data
#'
#' @param key Unique identifier for the cached data
#' @param data Data to cache (optional, if NULL will try to retrieve)
#' @param cache_dir Directory to store cache files (default: "spotify_cache")
#' @param expiry_hours Number of hours before cache expires (default: 24)
#' @param force_refresh Logical, whether to force refresh cached data (default: FALSE)
#'
#' @return Cached data if retrieving, invisible(TRUE) if storing
#' @export
#'
#' @examples
#' \dontrun{
#' # Cache some data
#' chart_data <- get_daily_charts("2025-09-01")
#' cache_data("charts_2025_09_01", chart_data)
#'
#' # Retrieve cached data
#' cached_charts <- cache_data("charts_2025_09_01")
#'
#' # Force refresh
#' fresh_data <- cache_data("charts_2025_09_01", force_refresh = TRUE)
#' }
cache_data <- function(key, data = NULL, cache_dir = "spotify_cache",
                      expiry_hours = 24, force_refresh = FALSE) {

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Generate cache file path
  cache_file <- file.path(cache_dir, paste0(make.names(key), ".rds"))

  # If data is provided, store it
  if (!is.null(data)) {
    tryCatch({
      saveRDS(list(
        data = data,
        timestamp = Sys.time(),
        key = key
      ), cache_file)
      message(glue::glue("Data cached: {key}"))
      return(invisible(TRUE))
    }, error = function(e) {
      warning(glue::glue("Failed to cache data: {e$message}"))
      return(invisible(FALSE))
    })
  }

  # Otherwise, try to retrieve cached data
  if (file.exists(cache_file) && !force_refresh) {
    tryCatch({
      cached <- readRDS(cache_file)

      # Check if cache has expired
      age_hours <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "hours"))

      if (age_hours <= expiry_hours) {
        message(glue::glue("Using cached data: {key} (age: {round(age_hours, 1)} hours)"))
        return(cached$data)
      } else {
        message(glue::glue("Cache expired for: {key} (age: {round(age_hours, 1)} hours)"))
        file.remove(cache_file)
      }
    }, error = function(e) {
      warning(glue::glue("Failed to read cache: {e$message}"))
      if (file.exists(cache_file)) file.remove(cache_file)
    })
  }

  return(NULL)
}

#' Clear Cache
#'
#' Removes cached files
#'
#' @param pattern Optional pattern to match specific cache files
#' @param cache_dir Directory containing cache files (default: "spotify_cache")
#' @param confirm Logical, whether to ask for confirmation (default: TRUE)
#'
#' @return Number of files removed
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' clear_cache()
#'
#' # Clear specific cache files
#' clear_cache(pattern = "charts_2025")
#' }
clear_cache <- function(pattern = NULL, cache_dir = "spotify_cache", confirm = TRUE) {

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(0)
  }

  # Find cache files
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (!is.null(pattern)) {
    cache_files <- cache_files[grepl(pattern, basename(cache_files))]
  }

  if (length(cache_files) == 0) {
    message("No cache files found")
    return(0)
  }

  # Ask for confirmation if requested
  if (confirm && interactive()) {
    response <- readline(glue::glue("Remove {length(cache_files)} cache files? (y/N): "))
    if (!tolower(response) %in% c("y", "yes")) {
      message("Cache clearing cancelled")
      return(0)
    }
  }

  # Remove files
  removed <- 0
  for (file in cache_files) {
    if (file.remove(file)) {
      removed <- removed + 1
    }
  }

  message(glue::glue("Removed {removed} cache files"))
  return(removed)
}

#' Validate Date Input
#'
#' Comprehensive date validation for chart functions
#'
#' @param date Date input to validate
#' @param allow_future Logical, whether to allow future dates (default: FALSE)
#' @param min_date Minimum allowed date (default: 2006-01-01, when Spotify launched)
#'
#' @return Validated Date object
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate a date string
#' valid_date <- validate_dates("2025-09-01")
#'
#' # Validate multiple dates
#' dates <- c("2025-09-01", "2025-09-02", "2025-09-03")
#' valid_dates <- sapply(dates, validate_dates)
#' }
validate_dates <- function(date, allow_future = FALSE, min_date = "2006-01-01") {

  tryCatch({
    # Convert to Date object
    validated_date <- as.Date(date)

    # Check for NA values
    if (is.na(validated_date)) {
      stop(glue::glue("Invalid date format: {date}"))
    }

    # Check minimum date
    min_date <- as.Date(min_date)
    if (validated_date < min_date) {
      stop(glue::glue("Date {validated_date} is before Spotify's launch date {min_date}"))
    }

    # Check future dates if not allowed
    if (!allow_future && validated_date > Sys.Date()) {
      stop(glue::glue("Future date not allowed: {validated_date}"))
    }

    return(validated_date)

  }, error = function(e) {
    stop(glue::glue("Date validation error: {e$message}"))
  })
}

#' Format Numbers for Display
#'
#' Formats large numbers in a human-readable way
#'
#' @param numbers Numeric vector to format
#' @param unit Character string for unit (default: "")
#' @param decimal_places Number of decimal places (default: 1)
#' @param use_si Logical, whether to use SI prefixes (K, M, B) (default: TRUE)
#'
#' @return Character vector of formatted numbers
#' @export
#'
#' @examples
#' \dontrun{
#' # Format large numbers
#' format_numbers(c(1500, 1500000, 1500000000))
#' # Returns: "1.5K", "1.5M", "1.5B"
#'
#' # Format with units
#' format_numbers(c(45, 120), unit = "days")
#' # Returns: "45 days", "120 days"
#' }
format_numbers <- function(numbers, unit = "", decimal_places = 1, use_si = TRUE) {

  if (!is.numeric(numbers)) {
    return(as.character(numbers))
  }

  formatted <- character(length(numbers))

  for (i in seq_along(numbers)) {
    num <- numbers[i]

    if (is.na(num)) {
      formatted[i] <- "N/A"
    } else if (use_si && abs(num) >= 1000) {
      # Use SI prefixes
      if (abs(num) >= 1e9) {
        formatted[i] <- sprintf("%.${decimal_places}fB", num / 1e9)
      } else if (abs(num) >= 1e6) {
        formatted[i] <- sprintf("%.${decimal_places}fM", num / 1e6)
      } else if (abs(num) >= 1e3) {
        formatted[i] <- sprintf("%.${decimal_places}fK", num / 1e3)
      } else {
        formatted[i] <- sprintf("%.${decimal_places}f", num)
      }
    } else {
      # Format without SI prefixes
      if (num == round(num)) {
        formatted[i] <- format(num, big.mark = ",", scientific = FALSE)
      } else {
        formatted[i] <- sprintf("%.${decimal_places}f", num)
      }
    }

    # Add unit if specified
    if (unit != "") {
      formatted[i] <- paste(formatted[i], unit)
    }
  }

  return(formatted)
}

#' Create Progress Bar
#'
#' Creates a simple progress bar for long-running operations
#'
#' @param total Total number of items to process
#' @param prefix Prefix text for progress bar
#' @param width Width of progress bar in characters (default: 50)
#'
#' @return Progress bar function
#' @export
#'
#' @examples
#' \dontrun{
#' # Create progress bar for 100 items
#' pb <- create_progress_bar(100, "Processing charts")
#'
#' # Update progress in a loop
#' for (i in 1:100) {
#'   # Do some work
#'   pb(i)
#'   Sys.sleep(0.1)
#' }
#' }
create_progress_bar <- function(total, prefix = "Progress", width = 50) {

  start_time <- Sys.time()

  progress_function <- function(current) {
    if (current > total) current <- total

    # Calculate progress
    percent <- current / total
    filled_length <- round(width * percent)

    # Create bar
    bar <- paste(c(
      rep("=", filled_length),
      rep("-", width - filled_length)
    ), collapse = "")

    # Calculate time estimates
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (current > 0) {
      estimated_total <- elapsed_time / current * total
      remaining_time <- estimated_total - elapsed_time
    } else {
      remaining_time <- 0
    }

    # Format output
    message <- sprintf(
      "\r%s: [%s] %d%% (%d/%d) - %s remaining",
      prefix,
      bar,
      round(percent * 100),
      current,
      total,
      format_time(remaining_time)
    )

    # Print without newline
    cat(message)

    # Add newline when complete
    if (current == total) {
      cat("\n")
    }
  }

  return(progress_function)
}

#' Format Time Duration
#'
#' Formats time duration in a human-readable way
#'
#' @param seconds Time duration in seconds
#'
#' @return Character string with formatted time
#' @keywords internal
format_time <- function(seconds) {
  if (is.na(seconds) || seconds <= 0) return("0s")

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)

  if (hours > 0) {
    return(sprintf("%dh %dm %ds", hours, minutes, secs))
  } else if (minutes > 0) {
    return(sprintf("%dm %ds", minutes, secs))
  } else {
    return(sprintf("%ds", secs))
  }
}

#' Safe HTTP Request
#'
#' Makes HTTP requests with comprehensive error handling
#'
#' @param url URL to request
#' @param method HTTP method (default: "GET")
#' @param headers Named list of headers
#' @param timeout Request timeout in seconds (default: 30)
#' @param retries Number of retry attempts (default: 3)
#'
#' @return httr2 response object or NULL if failed
#' @keywords internal
safe_http_request <- function(url, method = "GET", headers = NULL,
                             timeout = 30, retries = 3) {

  default_headers <- list(
    "User-Agent" = "R spotify package (research purposes)",
    "Accept" = "application/json"
  )

  # Merge headers
  if (!is.null(headers)) {
    headers <- utils::modifyList(default_headers, headers)
  } else {
    headers <- default_headers
  }

  for (attempt in 1:retries) {
    tryCatch({
      response <- httr2::request(url) %>%
        httr2::req_method(method) %>%
        httr2::req_headers(!!!headers) %>%
        httr2::req_timeout(timeout) %>%
        httr2::req_perform()

      return(response)

    }, error = function(e) {
      if (attempt == retries) {
        warning(glue::glue("HTTP request failed after {retries} attempts: {e$message}"))
        return(NULL)
      }
      # Wait before retry
      Sys.sleep(min(attempt * 2, 10))
    })
  }

  return(NULL)
}

#' Check Internet Connection
#'
#' Checks if internet connection is available
#'
#' @param timeout Timeout for connection check in seconds (default: 5)
#'
#' @return Logical, TRUE if connection is available
#' @export
#'
#' @examples
#' \dontrun{
#' if (check_internet()) {
#'   # Proceed with scraping
#'   data <- get_daily_charts("2025-09-01")
#' } else {
#'   # Use cached data or show error
#'   message("No internet connection available")
#' }
#' }
check_internet <- function(timeout = 5) {
  tryCatch({
    response <- httr2::request("https://httpbin.org/status/200") %>%
      httr2::req_timeout(timeout) %>%
      httr2::req_perform()

    return(httr2::resp_status(response) == 200)

  }, error = function(e) {
    return(FALSE)
  })
}

#' Get Package Version
#'
#' Returns the current package version
#'
#' @return Character string with package version
#' @export
#'
#' @examples
#' \dontrun{
#' get_package_version()
#' }
get_package_version <- function() {
  utils::packageVersion("spotify")
}

#' Setup Package Environment
#'
#' Initializes package environment and settings
#'
#' @param cache_dir Directory for caching (default: "spotify_cache")
#' @param default_delay Default delay between requests (default: 1)
#' @param quiet Logical, whether to suppress startup messages (default: FALSE)
#'
#' @return Invisible list with settings
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize package with custom settings
#' setup_package_environment(cache_dir = "my_spotify_cache", default_delay = 2)
#' }
setup_package_environment <- function(cache_dir = "spotify_cache",
                                     default_delay = 1,
                                     quiet = FALSE) {

  settings <- list(
    cache_dir = cache_dir,
    default_delay = default_delay,
    setup_time = Sys.time(),
    version = get_package_version()
  )

  # Create cache directory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    if (!quiet) {
      message(glue::glue("Created cache directory: {cache_dir}"))
    }
  }

  # Store settings in package environment
  assign("spotify_settings", settings, envir = .GlobalEnv)

  if (!quiet) {
    message(glue::glue("Spotify package v{settings$version} initialized"))
    message(glue::glue("Cache directory: {cache_dir}"))
    message(glue::glue("Default delay: {default_delay} seconds"))
  }

  return(invisible(settings))
}

#' Clean Up Package Environment
#'
#' Cleans up package environment and temporary files
#'
#' @param remove_cache Logical, whether to remove cache directory (default: FALSE)
#' @param quiet Logical, whether to suppress messages (default: FALSE)
#'
#' @return Invisible TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' # Clean up without removing cache
#' cleanup_package_environment()
#'
#' # Clean up and remove all cache
#' cleanup_package_environment(remove_cache = TRUE)
#' }
cleanup_package_environment <- function(remove_cache = FALSE, quiet = FALSE) {

  # Remove rate limiting environment
  if (exists("spotify_rate_limit", envir = .GlobalEnv)) {
    rm("spotify_rate_limit", envir = .GlobalEnv)
    if (!quiet) message("Cleared rate limiting cache")
  }

  # Remove settings
  if (exists("spotify_settings", envir = .GlobalEnv)) {
    settings <- get("spotify_settings", envir = .GlobalEnv)
    rm("spotify_settings", envir = .GlobalEnv)

    # Remove cache if requested
    if (remove_cache && dir.exists(settings$cache_dir)) {
      unlink(settings$cache_dir, recursive = TRUE)
      if (!quiet) message(glue::glue("Removed cache directory: {settings$cache_dir}"))
    }
  }

  if (!quiet) message("Package environment cleaned up")

  return(invisible(TRUE))
}
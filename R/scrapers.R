#' Get Daily Chart Data from Spotify
#'
#' Scrapes daily chart data from Spotify's charts API for a specific date
#'
#' @param date A Date object or character string (YYYY-MM-DD format) for the chart date
#' @param chart_type Character string specifying chart type. Options: "regional-global-daily",
#'   "regional-global-weekly", "viral-global-daily", "viral-global-weekly"
#' @param region Character string for region code (default: "global")
#' @param retries Number of retry attempts for failed requests (default: 3)
#' @param delay Seconds to wait between requests (default: 1)
#'
#' @return A tibble with chart data including track information, artists, and streaming metrics
#' @export
#'
#' @examples
#' \dontrun{
#' # Get global daily charts for a specific date
#' charts <- get_daily_charts("2025-09-01")
#'
#' # Get viral charts
#' viral <- get_daily_charts("2025-09-01", chart_type = "viral-global-daily")
#' }
get_daily_charts <- function(date,
                            chart_type = "regional-global-daily",
                            region = "global",
                            retries = 3,
                            delay = 1) {

  # Validate and format date
  formatted_date <- validate_chart_date(date)

  # Build URL
  base_url <- "https://charts-spotify-com-service.spotify.com/auth/v0/charts"
  url <- glue::glue("{base_url}/{chart_type}/{formatted_date}")

  if (region != "global" && grepl("regional", chart_type)) {
    url <- glue::glue("{base_url}/{chart_type}/{region}/{formatted_date}")
  }

  # Make request with retries
  tryCatch({
    response <- make_authenticated_spotify_request(url, retries = retries, delay = delay)

    if (is.null(response)) {
      warning(glue::glue("Failed to get data for date: {formatted_date}"))
      return(tibble::tibble())
    }

    # Parse response
    parsed_data <- parse_chart_response(response, date = formatted_date,
                                       chart_type = chart_type, region = region)

    return(parsed_data)

  }, error = function(e) {
    warning(glue::glue("Error getting charts for {formatted_date}: {e$message}"))
    return(tibble::tibble())
  })
}

#' Get Chart Data for Date Range
#'
#' Scrapes chart data for multiple dates within a specified range
#'
#' @param start_date Start date (Date object or YYYY-MM-DD string)
#' @param end_date End date (Date object or YYYY-MM-DD string)
#' @param chart_type Character string specifying chart type (default: "regional-global-daily")
#' @param region Character string for region code (default: "global")
#' @param delay Seconds to wait between requests (default: 1)
#' @param progress Logical, whether to show progress bar (default: TRUE)
#'
#' @return A tibble with chart data for all dates in the range
#' @export
#'
#' @examples
#' \dontrun{
#' # Get charts for a week
#' weekly_charts <- get_chart_range("2025-09-01", "2025-09-07")
#'
#' # Get charts for specific region
#' uk_charts <- get_chart_range("2025-09-01", "2025-09-07", region = "GB")
#' }
get_chart_range <- function(start_date, end_date,
                           chart_type = "regional-global-daily",
                           region = "global",
                           delay = 1,
                           progress = TRUE) {

  # Validate dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (start_date > end_date) {
    stop("start_date must be before or equal to end_date")
  }

  # Generate date sequence
  dates <- seq(from = start_date, to = end_date, by = "day")

  message(glue::glue("Scraping {length(dates)} days of chart data..."))

  # Initialize progress if requested
  if (progress && requireNamespace("utils", quietly = TRUE)) {
    pb <- utils::txtProgressBar(min = 0, max = length(dates), style = 3)
  }

  # Scrape data for each date
  all_data <- purrr::map_dfr(seq_along(dates), function(i) {
    date <- dates[i]

    # Update progress
    if (progress && exists("pb")) {
      utils::setTxtProgressBar(pb, i)
    }

    # Get data for this date
    data <- get_daily_charts(date,
                            chart_type = chart_type,
                            region = region,
                            retries = 2,
                            delay = delay)

    # Add small delay between requests
    if (delay > 0) Sys.sleep(delay)

    return(data)
  })

  # Close progress bar
  if (progress && exists("pb")) {
    close(pb)
  }

  message(glue::glue("Scraped {nrow(all_data)} chart entries"))

  return(all_data)
}

#' Get Regional Charts
#'
#' Convenience function to scrape charts for specific regions
#'
#' @param date Date for the chart (Date object or YYYY-MM-DD string)
#' @param regions Character vector of region codes (e.g., c("US", "GB", "DE"))
#' @param chart_type Character string specifying chart type (default: "regional-global-daily")
#' @param delay Seconds to wait between requests (default: 1)
#'
#' @return A tibble with chart data for all specified regions
#' @export
#'
#' @examples
#' \dontrun{
#' # Get charts for multiple regions
#' regional_data <- get_regional_charts("2025-09-01",
#'                                     regions = c("US", "GB", "DE"))
#' }
get_regional_charts <- function(date, regions,
                               chart_type = "regional-global-daily",
                               delay = 1) {

  message(glue::glue("Scraping regional charts for {length(regions)} regions..."))

  # Scrape data for each region
  all_data <- purrr::map_dfr(regions, function(region) {
    data <- get_daily_charts(date,
                            chart_type = chart_type,
                            region = region,
                            delay = delay)

    # Add small delay between requests
    if (delay > 0) Sys.sleep(delay)

    return(data)
  })

  message(glue::glue("Scraped {nrow(all_data)} regional chart entries"))

  return(all_data)
}

#' Validate Chart Date
#'
#' Internal function to validate and format dates for chart URLs
#'
#' @param date Date object or character string
#'
#' @return Formatted date string (YYYY-MM-DD)
#' @keywords internal
validate_chart_date <- function(date) {
  tryCatch({
    formatted_date <- as.Date(date)
    return(format(formatted_date, "%Y-%m-%d"))
  }, error = function(e) {
    stop(glue::glue("Invalid date format: {date}. Use YYYY-MM-DD format or Date object."))
  })
}



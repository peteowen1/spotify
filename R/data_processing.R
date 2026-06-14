#' Parse Chart Response from Spotify API
#'
#' Converts raw JSON response from Spotify charts API into a structured tibble
#'
#' @param response Raw JSON response from Spotify API
#' @param date Date string for the chart
#' @param chart_type Type of chart requested
#' @param region Region code for the chart
#'
#' @return A tibble with structured chart data
#' @export
#'
#' @examples
#' \dontrun{
#' # This function is typically called internally by scraper functions
#' # parsed <- parse_chart_response(response, "2025-09-01", "regional-global-daily", "global")
#' }
parse_chart_response <- function(response, date, chart_type, region = "global") {

  tryCatch({
    # Check if response has expected structure
    if (is.null(response) || !("entries" %in% names(response))) {
      warning("Response does not contain expected chart data structure")
      return(tibble::tibble())
    }

    # Extract chart entries
    entries <- response$entries

    if (length(entries) == 0) {
      message(glue::glue("No chart entries found for {date}"))
      return(tibble::tibble())
    }

    # Parse each entry into a structured format
    parsed_entries <- purrr::map_dfr(entries, function(entry) {
      parse_single_entry(entry)
    })

    # Add metadata columns
    parsed_entries <- parsed_entries %>%
      dplyr::mutate(
        chart_date = as.Date(date),
        chart_type = chart_type,
        region = region,
        scraped_at = Sys.time()
      )

    return(parsed_entries)

  }, error = function(e) {
    warning(glue::glue("Error parsing chart response: {e$message}"))
    return(tibble::tibble())
  })
}

#' Parse Single Chart Entry
#'
#' Internal function to parse a single chart entry from the API response
#'
#' @param entry Single chart entry from API response
#'
#' @return A tibble row with parsed entry data
#' @keywords internal
parse_single_entry <- function(entry) {

  tryCatch({
    # Extract basic chart information
    chart_data <- entry$chartEntryData %||% list()
    chart_position <- chart_data$currentRank %||% NA_integer_
    previous_position <- chart_data$previousRank %||% NA_integer_
    peak_position <- chart_data$peakRank %||% NA_integer_
    total_appearances <- chart_data$appearancesOnChart %||% NA_integer_
    consecutive_appearances <- chart_data$consecutiveAppearancesOnChart %||% NA_integer_
    peak_date <- chart_data$peakDate %||% NA_character_
    entry_status <- chart_data$entryStatus %||% NA_character_
    entry_date <- chart_data$entryDate %||% NA_character_

    # Extract streams data
    ranking_metric <- chart_data$rankingMetric %||% list()
    streams_raw <- ranking_metric$value %||% NA_integer_
    streams_formatted <- ranking_metric$formattedValue %||% NA_character_

    # Convert streams to numeric if it's a character string
    streams <- if (is.character(streams_raw)) {
      # Remove commas and convert to numeric
      as.numeric(gsub(",", "", streams_raw))
    } else {
      as.numeric(streams_raw)
    }

    # Extract track information
    track_data <- entry$trackMetadata %||% list()
    track_name <- track_data$trackName %||% NA_character_
    track_uri <- track_data$trackUri %||% NA_character_
    display_image_uri <- track_data$displayImageUri %||% NA_character_
    release_date <- track_data$releaseDate %||% NA_character_

    # Extract artist information
    artists_data <- track_data$artists %||% list()
    if (length(artists_data) > 0) {
      main_artist <- artists_data[[1]]$name %||% NA_character_
      all_artists <- purrr::map_chr(artists_data, ~ .x$name %||% NA_character_)
      all_artists_string <- paste(all_artists, collapse = ", ")
    } else {
      main_artist <- NA_character_
      all_artists_string <- NA_character_
    }

    # Extract labels and other metadata
    labels_data <- track_data$labels %||% list()
    record_label <- if (length(labels_data) > 0) labels_data[[1]]$name %||% NA_character_ else NA_character_

    # Create structured tibble row
    tibble::tibble(
      chart_position = chart_position,
      previous_position = previous_position,
      peak_position = peak_position,
      total_appearances = total_appearances,
      consecutive_appearances = consecutive_appearances,
      peak_date = as.Date(peak_date),
      entry_status = entry_status,
      entry_date = as.Date(entry_date),
      streams = streams,
      streams_formatted = streams_formatted,
      track_name = track_name,
      track_uri = track_uri,
      main_artist = main_artist,
      all_artists = all_artists_string,
      display_image_uri = display_image_uri,
      release_date = as.Date(release_date),
      record_label = record_label
    )

  }, error = function(e) {
    # Return empty row if parsing fails
    tibble::tibble(
      chart_position = NA_integer_,
      previous_position = NA_integer_,
      peak_position = NA_integer_,
      total_appearances = NA_integer_,
      consecutive_appearances = NA_integer_,
      peak_date = as.Date(NA),
      entry_status = NA_character_,
      entry_date = as.Date(NA),
      streams = NA_integer_,
      streams_formatted = NA_character_,
      track_name = NA_character_,
      track_uri = NA_character_,
      main_artist = NA_character_,
      all_artists = NA_character_,
      display_image_uri = NA_character_,
      release_date = as.Date(NA),
      record_label = NA_character_
    )
  })
}

#' Clean Spotify Chart Data
#'
#' Standardizes and cleans scraped Spotify chart data
#'
#' @param data Raw chart data tibble from scraper functions
#' @param remove_duplicates Logical, whether to remove duplicate entries (default: TRUE)
#' @param standardize_names Logical, whether to standardize artist/track names (default: TRUE)
#'
#' @return A cleaned tibble with standardized data
#' @export
#'
#' @examples
#' \dontrun{
#' # Clean scraped data
#' raw_data <- get_daily_charts("2025-09-01")
#' clean_data <- clean_spotify_data(raw_data)
#' }
clean_spotify_data <- function(data, remove_duplicates = TRUE, standardize_names = TRUE) {

  if (nrow(data) == 0) {
    return(data)
  }

  cleaned_data <- data

  # Standardize text fields
  if (standardize_names) {
    cleaned_data <- cleaned_data %>%
      dplyr::mutate(
        track_name = clean_text_field(.data$track_name),
        main_artist = clean_text_field(.data$main_artist),
        all_artists = clean_text_field(.data$all_artists)
      )
  }

  # Handle missing values and data types
  cleaned_data <- cleaned_data %>%
    dplyr::mutate(
      chart_position = as.integer(.data$chart_position),
      previous_position = as.integer(.data$previous_position),
      peak_position = as.integer(.data$peak_position),
      days_on_chart = as.integer(.data$days_on_chart),
      chart_date = as.Date(.data$chart_date),
      release_date = as.Date(.data$release_date)
    )

  # Calculate position change
  cleaned_data <- cleaned_data %>%
    dplyr::mutate(
      position_change = dplyr::case_when(
        is.na(.data$previous_position) ~ "new",
        .data$previous_position > .data$chart_position ~ "up",
        .data$previous_position < .data$chart_position ~ "down",
        .data$previous_position == .data$chart_position ~ "same",
        TRUE ~ "unknown"
      ),
      position_change_numeric = dplyr::case_when(
        is.na(.data$previous_position) ~ 0L,
        TRUE ~ .data$previous_position - .data$chart_position
      ),
      # Use consecutive_appearances as days_on_chart for compatibility
      days_on_chart = .data$consecutive_appearances
    )

  # Remove duplicates if requested
  if (remove_duplicates) {
    cleaned_data <- cleaned_data %>%
      dplyr::distinct(.data$track_uri, .data$chart_date, .data$region, .keep_all = TRUE)
  }

  # Sort by chart position
  cleaned_data <- cleaned_data %>%
    dplyr::arrange(.data$chart_date, .data$chart_position)

  return(cleaned_data)
}

#' Clean Text Field
#'
#' Internal function to clean and standardize text fields
#'
#' @param text Character vector to clean
#'
#' @return Cleaned character vector
#' @keywords internal
clean_text_field <- function(text) {
  if (is.null(text) || all(is.na(text))) {
    return(text)
  }

  # Remove extra whitespace and standardize
  cleaned <- text %>%
    stringr::str_trim() %>%
    stringr::str_squish()

  # Replace empty strings with NA
  cleaned[cleaned == "" | cleaned == "Unknown"] <- NA_character_

  return(cleaned)
}

#' Aggregate Streaming Data
#'
#' Aggregates chart data by various dimensions (artist, track, time period)
#'
#' @param data Cleaned chart data tibble
#' @param by Character string specifying aggregation level: "artist", "track", "date", or "month"
#' @param metric Character string specifying metric to calculate: "appearances", "peak_position", "avg_position"
#'
#' @return Aggregated tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Aggregate by artist to see total chart appearances
#' artist_stats <- aggregate_streams(clean_data, by = "artist", metric = "appearances")
#'
#' # Get peak positions by track
#' track_peaks <- aggregate_streams(clean_data, by = "track", metric = "peak_position")
#' }
aggregate_streams <- function(data, by = "artist", metric = "appearances") {

  if (nrow(data) == 0) {
    return(tibble::tibble())
  }

  # Define grouping variables
  group_vars <- switch(by,
    "artist" = c("main_artist"),
    "track" = c("track_name", "main_artist", "track_uri"),
    "date" = c("chart_date"),
    "month" = c("chart_year_month"),
    "region" = c("region"),
    stop(glue::glue("Invalid 'by' parameter: {by}. Use 'artist', 'track', 'date', 'month', or 'region'"))
  )

  # Add derived grouping variables if needed
  if (by == "month") {
    data <- data %>%
      dplyr::mutate(chart_year_month = format(.data$chart_date, "%Y-%m"))
  }

  # Perform aggregation
  result <- switch(metric,
    "appearances" = aggregate_appearances(data, group_vars),
    "peak_position" = aggregate_peak_position(data, group_vars),
    "avg_position" = aggregate_avg_position(data, group_vars),
    "days_on_chart" = aggregate_days_on_chart(data, group_vars),
    stop(glue::glue("Invalid 'metric' parameter: {metric}. Use 'appearances', 'peak_position', 'avg_position', or 'days_on_chart'"))
  )

  return(result)
}

#' Aggregate Chart Appearances
#'
#' Internal function to count chart appearances
#'
#' @param data Chart data
#' @param group_vars Grouping variables
#'
#' @return Aggregated tibble
#' @keywords internal
aggregate_appearances <- function(data, group_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      total_appearances = dplyr::n(),
      unique_charts = dplyr::n_distinct(.data$chart_date, .data$region),
      first_appearance = min(.data$chart_date, na.rm = TRUE),
      last_appearance = max(.data$chart_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_appearances))
}

#' Aggregate Peak Positions
#'
#' Internal function to find peak chart positions
#'
#' @param data Chart data
#' @param group_vars Grouping variables
#'
#' @return Aggregated tibble
#' @keywords internal
aggregate_peak_position <- function(data, group_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      peak_position = min(.data$chart_position, na.rm = TRUE),
      peak_date = .data$chart_date[which.min(.data$chart_position)][1],
      total_appearances = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$peak_position)
}

#' Aggregate Average Positions
#'
#' Internal function to calculate average chart positions
#'
#' @param data Chart data
#' @param group_vars Grouping variables
#'
#' @return Aggregated tibble
#' @keywords internal
aggregate_avg_position <- function(data, group_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      median_position = median(.data$chart_position, na.rm = TRUE),
      total_appearances = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$avg_position)
}

#' Aggregate Days on Chart
#'
#' Internal function to calculate total days on chart
#'
#' @param data Chart data
#' @param group_vars Grouping variables
#'
#' @return Aggregated tibble
#' @keywords internal
aggregate_days_on_chart <- function(data, group_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      total_days_on_chart = sum(.data$days_on_chart, na.rm = TRUE),
      max_days_on_chart = max(.data$days_on_chart, na.rm = TRUE),
      avg_days_on_chart = mean(.data$days_on_chart, na.rm = TRUE),
      total_appearances = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_days_on_chart))
}

#' Calculate Chart Positions and Movements
#'
#' Calculates detailed chart position analytics including trends and movements
#'
#' @param data Cleaned chart data
#' @param track_uri Specific track URI to analyze (optional)
#' @param artist_name Specific artist to analyze (optional)
#'
#' @return Tibble with chart position analytics
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze chart movements for all tracks
#' movements <- calculate_chart_positions(clean_data)
#'
#' # Analyze specific artist's chart performance
#' artist_movements <- calculate_chart_positions(clean_data, artist_name = "Taylor Swift")
#' }
calculate_chart_positions <- function(data, track_uri = NULL, artist_name = NULL) {

  # Filter data if specific track or artist requested
  if (!is.null(track_uri)) {
    data <- data %>% dplyr::filter(.data$track_uri == !!track_uri)
  }

  if (!is.null(artist_name)) {
    data <- data %>% dplyr::filter(.data$main_artist == !!artist_name)
  }

  if (nrow(data) == 0) {
    return(tibble::tibble())
  }

  # Calculate chart position analytics
  position_analytics <- data %>%
    dplyr::group_by(.data$track_uri, .data$track_name, .data$main_artist) %>%
    dplyr::arrange(.data$chart_date) %>%
    dplyr::mutate(
      # Calculate position trends
      position_trend = calculate_trend(.data$chart_position),
      # Days since last appearance (for re-entries)
      days_since_last = as.numeric(.data$chart_date - dplyr::lag(.data$chart_date, default = .data$chart_date[1])),
      # Track consecutive appearances
      is_consecutive = .data$days_since_last <= 1,
      consecutive_run = cumsum(!.data$is_consecutive)
    ) %>%
    dplyr::summarise(
      # Position statistics
      peak_position = min(.data$chart_position, na.rm = TRUE),
      worst_position = max(.data$chart_position, na.rm = TRUE),
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      median_position = median(.data$chart_position, na.rm = TRUE),

      # Time statistics
      total_days = dplyr::n(),
      first_chart_date = min(.data$chart_date),
      last_chart_date = max(.data$chart_date),
      chart_span_days = as.numeric(max(.data$chart_date) - min(.data$chart_date)) + 1,

      # Movement statistics
      total_upward_moves = sum(.data$position_change == "up", na.rm = TRUE),
      total_downward_moves = sum(.data$position_change == "down", na.rm = TRUE),
      biggest_jump = max(.data$position_change_numeric, na.rm = TRUE),
      biggest_drop = min(.data$position_change_numeric, na.rm = TRUE),

      # Consistency metrics
      position_volatility = sd(.data$chart_position, na.rm = TRUE),
      longest_consecutive_run = max(rle(.data$consecutive_run)$lengths),

      .groups = "drop"
    ) %>%
    # Calculate derived metrics
    dplyr::mutate(
      chart_efficiency = .data$total_days / .data$chart_span_days,
      position_improvement = .data$worst_position - .data$peak_position,
      net_movement = .data$total_upward_moves - .data$total_downward_moves
    ) %>%
    dplyr::arrange(.data$peak_position, dplyr::desc(.data$total_days))

  return(position_analytics)
}

#' Calculate Position Trend
#'
#' Internal function to calculate trend in chart positions
#'
#' @param positions Vector of chart positions
#'
#' @return Character string indicating trend
#' @keywords internal
calculate_trend <- function(positions) {
  if (length(positions) < 2) return("stable")

  # Simple linear trend calculation
  trend_slope <- lm(positions ~ seq_along(positions))$coefficients[2]

  dplyr::case_when(
    trend_slope < -0.5 ~ "improving",  # positions getting lower (better)
    trend_slope > 0.5 ~ "declining",   # positions getting higher (worse)
    TRUE ~ "stable"
  )
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
#' Find Top Artists by Chart Performance
#'
#' Analyzes chart data to identify top-performing artists based on various metrics
#'
#' @param data Cleaned chart data tibble
#' @param metric Character string specifying ranking metric: "appearances", "peak_positions",
#'   "avg_position", "days_on_chart", or "chart_dominance"
#' @param time_period Character string for time filtering: "all", "year", "month", or custom date range
#' @param year Numeric year for filtering (when time_period = "year")
#' @param month Numeric month for filtering (when time_period = "month")
#' @param start_date Start date for custom date range (Date object or YYYY-MM-DD)
#' @param end_date End date for custom date range (Date object or YYYY-MM-DD)
#' @param top_n Number of top artists to return (default: 10)
#'
#' @return A tibble with top artists ranked by the specified metric
#' @export
#'
#' @examples
#' \dontrun{
#' # Find artists with most chart appearances this year
#' top_artists_2025 <- top_artists(data, metric = "appearances", time_period = "year", year = 2025)
#'
#' # Find artists with best average chart positions
#' best_performers <- top_artists(data, metric = "avg_position", top_n = 20)
#'
#' # Find top artists in a specific date range
#' summer_tops <- top_artists(data, metric = "chart_dominance",
#'                           start_date = "2025-06-01", end_date = "2025-08-31")
#' }
top_artists <- function(data, metric = "appearances", time_period = "all",
                       year = NULL, month = NULL, start_date = NULL, end_date = NULL,
                       top_n = 10) {

  # Filter data by time period
  filtered_data <- filter_by_time_period(data, time_period, year, month, start_date, end_date)

  if (nrow(filtered_data) == 0) {
    warning("No data found for specified time period")
    return(tibble::tibble())
  }

  # Calculate artist rankings based on metric
  result <- switch(metric,
    "appearances" = calculate_artist_appearances(filtered_data),
    "peak_positions" = calculate_artist_peaks(filtered_data),
    "avg_position" = calculate_artist_avg_positions(filtered_data),
    "days_on_chart" = calculate_artist_days_on_chart(filtered_data),
    "chart_dominance" = calculate_artist_dominance(filtered_data),
    stop(glue::glue("Invalid metric: {metric}. Use 'appearances', 'peak_positions', 'avg_position', 'days_on_chart', or 'chart_dominance'"))
  )

  # Add time period metadata
  result <- result %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      analysis_period = describe_time_period(time_period, year, month, start_date, end_date),
      metric_used = metric
    )

  return(result)
}

#' Track Trends Over Time
#'
#' Analyzes trends in chart performance over time for artists or tracks
#'
#' @param data Cleaned chart data tibble
#' @param level Character string: "artist" or "track"
#' @param entity_name Name of the specific artist or track to analyze
#' @param time_unit Character string: "day", "week", or "month"
#' @param min_appearances Minimum number of chart appearances to include (default: 5)
#'
#' @return A tibble with time-based trend analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze Taylor Swift's chart trends by month
#' taylor_trends <- track_trends(data, level = "artist", entity_name = "Taylor Swift",
#'                              time_unit = "month")
#'
#' # Analyze daily trends for a specific track
#' song_trends <- track_trends(data, level = "track", entity_name = "Anti-Hero",
#'                            time_unit = "day")
#' }
track_trends <- function(data, level = "artist", entity_name = NULL,
                        time_unit = "month", min_appearances = 5) {

  # Filter data by entity if specified
  if (!is.null(entity_name)) {
    if (level == "artist") {
      data <- data %>% dplyr::filter(.data$main_artist == !!entity_name)
    } else if (level == "track") {
      data <- data %>% dplyr::filter(.data$track_name == !!entity_name)
    }
  }

  if (nrow(data) == 0) {
    warning(glue::glue("No data found for {entity_name}"))
    return(tibble::tibble())
  }

  # Create time grouping variable
  data <- data %>%
    dplyr::mutate(
      time_group = switch(time_unit,
        "day" = .data$chart_date,
        "week" = lubridate::floor_date(.data$chart_date, "week"),
        "month" = lubridate::floor_date(.data$chart_date, "month"),
        stop(glue::glue("Invalid time_unit: {time_unit}"))
      )
    )

  # Group by entity and time period
  grouping_vars <- c("time_group")
  if (level == "artist") {
    grouping_vars <- c(grouping_vars, "main_artist")
  } else {
    grouping_vars <- c(grouping_vars, "track_name", "main_artist", "track_uri")
  }

  # Calculate trends
  trends <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::summarise(
      appearances = dplyr::n(),
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      best_position = min(.data$chart_position, na.rm = TRUE),
      worst_position = max(.data$chart_position, na.rm = TRUE),
      total_days_on_chart = sum(.data$days_on_chart, na.rm = TRUE),
      unique_charts = dplyr::n_distinct(.data$chart_date, .data$region),
      .groups = "drop"
    ) %>%
    # Filter by minimum appearances
    dplyr::filter(.data$appearances >= min_appearances) %>%
    # Calculate trend metrics
    dplyr::arrange(.data$time_group) %>%
    dplyr::mutate(
      position_trend = calculate_position_trend(.data$avg_position),
      momentum = calculate_momentum(.data$appearances),
      consistency_score = calculate_consistency(.data$avg_position, .data$appearances)
    )

  return(trends)
}

#' Analyze Chart Performance
#'
#' Comprehensive analysis of chart performance including rankings, movements, and achievements
#'
#' @param data Cleaned chart data tibble
#' @param analysis_type Character string: "overview", "movements", "achievements", or "comparative"
#' @param entity_filter Optional filter for specific artist or track
#' @param region_filter Optional filter for specific region
#'
#' @return A list containing various performance analytics
#' @export
#'
#' @examples
#' \dontrun{
#' # Get comprehensive chart overview
#' overview <- chart_performance(data, analysis_type = "overview")
#'
#' # Analyze chart movements in detail
#' movements <- chart_performance(data, analysis_type = "movements")
#'
#' # Focus on specific artist's achievements
#' artist_analysis <- chart_performance(data, analysis_type = "achievements",
#'                                     entity_filter = "Drake")
#' }
chart_performance <- function(data, analysis_type = "overview",
                             entity_filter = NULL, region_filter = NULL) {

  # Apply filters if specified
  if (!is.null(entity_filter)) {
    data <- data %>%
      dplyr::filter(.data$main_artist == !!entity_filter |
                   .data$track_name == !!entity_filter)
  }

  if (!is.null(region_filter)) {
    data <- data %>% dplyr::filter(.data$region == !!region_filter)
  }

  # Perform analysis based on type
  result <- switch(analysis_type,
    "overview" = analyze_chart_overview(data),
    "movements" = analyze_chart_movements(data),
    "achievements" = analyze_achievements(data),
    "comparative" = analyze_comparative_performance(data),
    stop(glue::glue("Invalid analysis_type: {analysis_type}"))
  )

  return(result)
}

#' Generate Streaming Insights
#'
#' Creates comprehensive insights and summary statistics from chart data
#'
#' @param data Cleaned chart data tibble
#' @param insight_type Character string: "summary", "patterns", "anomalies", or "predictions"
#' @param time_window Number of days to analyze for recent trends (default: 30)
#'
#' @return A list containing insights and summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Get summary insights
#' insights <- streaming_insights(data, insight_type = "summary")
#'
#' # Identify patterns in the data
#' patterns <- streaming_insights(data, insight_type = "patterns")
#'
#' # Find anomalies and unusual chart behavior
#' anomalies <- streaming_insights(data, insight_type = "anomalies")
#' }
streaming_insights <- function(data, insight_type = "summary", time_window = 30) {

  if (nrow(data) == 0) {
    return(list(message = "No data available for analysis"))
  }

  # Generate insights based on type
  insights <- switch(insight_type,
    "summary" = generate_summary_insights(data),
    "patterns" = identify_chart_patterns(data),
    "anomalies" = detect_anomalies(data),
    "predictions" = generate_trend_predictions(data, time_window),
    stop(glue::glue("Invalid insight_type: {insight_type}"))
  )

  # Add metadata
  insights$analysis_info <- list(
    total_records = nrow(data),
    date_range = list(
      start = min(data$chart_date, na.rm = TRUE),
      end = max(data$chart_date, na.rm = TRUE)
    ),
    unique_artists = length(unique(data$main_artist)),
    unique_tracks = length(unique(data$track_uri)),
    regions_covered = unique(data$region),
    analysis_timestamp = Sys.time()
  )

  return(insights)
}

# Internal helper functions

#' Filter Data by Time Period
#' @keywords internal
filter_by_time_period <- function(data, time_period, year, month, start_date, end_date) {
  switch(time_period,
    "all" = data,
    "year" = {
      if (is.null(year)) stop("Year must be specified for time_period = 'year'")
      data %>% dplyr::filter(lubridate::year(.data$chart_date) == !!year)
    },
    "month" = {
      if (is.null(year) || is.null(month)) stop("Year and month must be specified for time_period = 'month'")
      data %>% dplyr::filter(
        lubridate::year(.data$chart_date) == !!year,
        lubridate::month(.data$chart_date) == !!month
      )
    },
    "custom" = {
      if (is.null(start_date) || is.null(end_date)) stop("start_date and end_date must be specified for custom time period")
      data %>% dplyr::filter(
        .data$chart_date >= as.Date(start_date),
        .data$chart_date <= as.Date(end_date)
      )
    },
    data
  )
}

#' Calculate Artist Chart Appearances
#' @keywords internal
calculate_artist_appearances <- function(data) {
  data %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      total_appearances = dplyr::n(),
      unique_tracks = dplyr::n_distinct(.data$track_uri),
      chart_span_days = as.numeric(max(.data$chart_date) - min(.data$chart_date)) + 1,
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      best_position = min(.data$chart_position, na.rm = TRUE),
      chart_consistency = dplyr::n() / chart_span_days,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_appearances))
}

#' Calculate Artist Peak Positions
#' @keywords internal
calculate_artist_peaks <- function(data) {
  data %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      best_peak_position = min(.data$chart_position, na.rm = TRUE),
      number_one_hits = sum(.data$chart_position == 1, na.rm = TRUE),
      top_10_hits = sum(.data$chart_position <= 10, na.rm = TRUE),
      top_50_hits = sum(.data$chart_position <= 50, na.rm = TRUE),
      total_appearances = dplyr::n(),
      unique_tracks = dplyr::n_distinct(.data$track_uri),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$best_peak_position, dplyr::desc(.data$number_one_hits))
}

#' Calculate Artist Average Positions
#' @keywords internal
calculate_artist_avg_positions <- function(data) {
  data %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      median_position = median(.data$chart_position, na.rm = TRUE),
      position_std = sd(.data$chart_position, na.rm = TRUE),
      total_appearances = dplyr::n(),
      consistency_score = 1 / (1 + sd(.data$chart_position, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$avg_position)
}

#' Calculate Artist Days on Chart
#' @keywords internal
calculate_artist_days_on_chart <- function(data) {
  data %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      total_days_on_chart = sum(.data$days_on_chart, na.rm = TRUE),
      avg_days_per_track = mean(.data$days_on_chart, na.rm = TRUE),
      max_days_single_track = max(.data$days_on_chart, na.rm = TRUE),
      total_appearances = dplyr::n(),
      longevity_score = sum(.data$days_on_chart, na.rm = TRUE) / dplyr::n_distinct(.data$track_uri),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_days_on_chart))
}

#' Calculate Artist Chart Dominance
#' @keywords internal
calculate_artist_dominance <- function(data) {
  data %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      dominance_score = calculate_dominance_score(.data$chart_position, .data$days_on_chart),
      weighted_position = sum(1 / .data$chart_position, na.rm = TRUE),
      chart_presence = dplyr::n() / max(dplyr::n()),
      peak_achievement = min(.data$chart_position, na.rm = TRUE),
      sustained_success = mean(.data$days_on_chart, na.rm = TRUE),
      total_appearances = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$dominance_score))
}

#' Calculate Dominance Score
#' @keywords internal
calculate_dominance_score <- function(positions, days_on_chart) {
  # Weighted score based on position (lower is better) and longevity
  position_score <- sum(1 / positions, na.rm = TRUE)
  longevity_score <- sum(days_on_chart, na.rm = TRUE)
  combined_score <- position_score * log(longevity_score + 1)
  return(combined_score)
}

#' Describe Time Period
#' @keywords internal
describe_time_period <- function(time_period, year, month, start_date, end_date) {
  switch(time_period,
    "all" = "All available data",
    "year" = glue::glue("Year {year}"),
    "month" = glue::glue("{month}/{year}"),
    "custom" = glue::glue("{start_date} to {end_date}"),
    "All data"
  )
}

#' Calculate Position Trend
#' @keywords internal
calculate_position_trend <- function(avg_positions) {
  if (length(avg_positions) < 2) return("stable")

  first_half <- mean(avg_positions[1:ceiling(length(avg_positions)/2)], na.rm = TRUE)
  second_half <- mean(avg_positions[ceiling(length(avg_positions)/2):length(avg_positions)], na.rm = TRUE)

  if (second_half < first_half - 2) return("improving")
  if (second_half > first_half + 2) return("declining")
  return("stable")
}

#' Calculate Momentum
#' @keywords internal
calculate_momentum <- function(appearances) {
  if (length(appearances) < 2) return(0)

  recent <- mean(tail(appearances, 3))
  earlier <- mean(head(appearances, 3))
  return(recent - earlier)
}

#' Calculate Consistency
#' @keywords internal
calculate_consistency <- function(avg_positions, appearances) {
  position_consistency <- 1 / (1 + sd(avg_positions, na.rm = TRUE))
  appearance_consistency <- min(appearances) / max(appearances)
  return((position_consistency + appearance_consistency) / 2)
}

#' Analyze Chart Overview
#' @keywords internal
analyze_chart_overview <- function(data) {
  list(
    total_entries = nrow(data),
    unique_artists = length(unique(data$main_artist)),
    unique_tracks = length(unique(data$track_uri)),
    date_range = range(data$chart_date),
    avg_position = mean(data$chart_position, na.rm = TRUE),
    position_distribution = table(cut(data$chart_position, breaks = c(0, 10, 50, 100, 200))),
    most_frequent_artists = head(sort(table(data$main_artist), decreasing = TRUE), 10)
  )
}

#' Analyze Chart Movements
#' @keywords internal
analyze_chart_movements <- function(data) {
  movements <- data %>%
    dplyr::group_by(.data$track_uri, .data$track_name, .data$main_artist) %>%
    dplyr::arrange(.data$chart_date) %>%
    dplyr::summarise(
      total_movements = dplyr::n() - 1,
      upward_movements = sum(.data$position_change == "up", na.rm = TRUE),
      downward_movements = sum(.data$position_change == "down", na.rm = TRUE),
      biggest_jump = max(.data$position_change_numeric, na.rm = TRUE),
      biggest_drop = min(.data$position_change_numeric, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    movement_summary = movements,
    biggest_jumps = movements %>% dplyr::arrange(dplyr::desc(.data$biggest_jump)) %>% dplyr::slice_head(n = 10),
    biggest_drops = movements %>% dplyr::arrange(.data$biggest_drop) %>% dplyr::slice_head(n = 10)
  ))
}

#' Analyze Achievements
#' @keywords internal
analyze_achievements <- function(data) {
  list(
    number_one_hits = data %>% dplyr::filter(.data$chart_position == 1) %>%
      dplyr::distinct(.data$track_uri, .keep_all = TRUE),
    longest_chart_runs = data %>%
      dplyr::group_by(.data$track_uri, .data$track_name, .data$main_artist) %>%
      dplyr::summarise(max_days = max(.data$days_on_chart, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$max_days)) %>%
      dplyr::slice_head(n = 20),
    most_appearances = data %>%
      dplyr::group_by(.data$main_artist) %>%
      dplyr::summarise(appearances = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$appearances)) %>%
      dplyr::slice_head(n = 20)
  )
}

#' Analyze Comparative Performance
#' @keywords internal
analyze_comparative_performance <- function(data) {
  by_region <- data %>%
    dplyr::group_by(.data$region, .data$main_artist) %>%
    dplyr::summarise(
      appearances = dplyr::n(),
      avg_position = mean(.data$chart_position, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    regional_comparison = by_region,
    cross_regional_artists = by_region %>%
      dplyr::group_by(.data$main_artist) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::arrange(.data$main_artist, .data$region)
  ))
}

#' Generate Summary Insights
#' @keywords internal
generate_summary_insights <- function(data) {
  list(
    key_metrics = list(
      total_chart_entries = nrow(data),
      unique_artists = length(unique(data$main_artist)),
      unique_tracks = length(unique(data$track_uri)),
      most_successful_artist = names(sort(table(data$main_artist), decreasing = TRUE))[1],
      highest_charting_song = data[which.min(data$chart_position), ]$track_name[1],
      average_chart_position = round(mean(data$chart_position, na.rm = TRUE), 1)
    ),
    time_span = list(
      start_date = min(data$chart_date),
      end_date = max(data$chart_date),
      total_days = as.numeric(max(data$chart_date) - min(data$chart_date)) + 1
    ),
    regional_coverage = table(data$region)
  )
}

#' Identify Chart Patterns
#' @keywords internal
identify_chart_patterns <- function(data) {
  # This would contain more sophisticated pattern recognition
  list(
    message = "Pattern identification functionality would be implemented here",
    seasonal_trends = "Seasonal analysis would be performed",
    genre_patterns = "Genre-based pattern analysis would be included"
  )
}

#' Detect Anomalies
#' @keywords internal
detect_anomalies <- function(data) {
  # This would contain anomaly detection algorithms
  list(
    message = "Anomaly detection functionality would be implemented here",
    unusual_movements = "Unusual chart movements would be identified",
    outlier_performances = "Outlier performances would be flagged"
  )
}

#' Generate Trend Predictions
#' @keywords internal
generate_trend_predictions <- function(data, time_window) {
  # This would contain predictive modeling
  list(
    message = "Trend prediction functionality would be implemented here",
    time_window_used = time_window,
    predicted_trends = "Future chart trends would be predicted based on historical data"
  )
}
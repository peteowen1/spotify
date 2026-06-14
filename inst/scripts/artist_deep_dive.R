#!/usr/bin/env Rscript

# Spotify Artist Deep Dive Analysis Script
# This script provides comprehensive analysis for a specific artist

# Load required packages
library(spotify)
library(dplyr)
library(ggplot2)
library(lubridate)

# Configuration - Change these parameters
ARTIST_NAME <- "Taylor Swift"  # Change to analyze different artist
START_DATE <- "2025-01-01"
END_DATE <- "2025-12-31"
OUTPUT_DIR <- paste0("artist_analysis_", make.names(ARTIST_NAME))

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("=== Spotify Artist Deep Dive ===\n")
cat("Artist:", ARTIST_NAME, "\n")
cat("Analysis period:", START_DATE, "to", END_DATE, "\n\n")

# Check internet connection
if (!check_internet()) {
  stop("Internet connection required for data scraping")
}

# Set up package environment
setup_package_environment(quiet = TRUE)

# Function to scrape artist data
scrape_artist_data <- function(artist_name, start_date, end_date) {
  cache_key <- paste0("artist_", make.names(artist_name), "_",
                     gsub("-", "", start_date), "_", gsub("-", "", end_date))

  # Try cache first
  cached_data <- cache_data(cache_key)
  if (!is.null(cached_data)) {
    cat("Using cached data for", artist_name, "\n")
    return(cached_data)
  }

  cat("Scraping data for", artist_name, "...\n")

  # For demonstration, scrape weekly data
  dates <- seq(from = as.Date(start_date),
               to = as.Date(end_date),
               by = "week")

  all_data <- tibble()

  for (i in seq_along(dates)) {
    if (i %% 10 == 0) {
      cat("Progress:", i, "/", length(dates), "dates\n")
    }

    daily_data <- get_daily_charts(
      date = dates[i],
      chart_type = "regional-global-daily",
      region = "global",
      delay = 1
    )

    if (nrow(daily_data) > 0) {
      all_data <- bind_rows(all_data, daily_data)
    }
  }

  # Filter for the specific artist
  artist_data <- all_data %>%
    filter(stringr::str_detect(main_artist, fixed(artist_name, ignore_case = TRUE)))

  # Cache the results
  cache_data(cache_key, artist_data)

  return(artist_data)
}

# 1. Data Collection
cat("1. Collecting chart data for", ARTIST_NAME, "...\n")
artist_data <- scrape_artist_data(ARTIST_NAME, START_DATE, END_DATE)

if (nrow(artist_data) == 0) {
  cat("No data found for", ARTIST_NAME, "in the specified period.\n")
  cat("This could be because:\n")
  cat("- The artist name doesn't match exactly\n")
  cat("- The artist had no chart entries in the sample dates\n")
  cat("- The scraping failed\n")
  cat("\nTry adjusting the ARTIST_NAME or expanding the date range.\n")
  quit(save = "no", status = 0)
}

cat("Found", nrow(artist_data), "chart entries for", ARTIST_NAME, "\n")

# Clean the data
clean_data <- clean_spotify_data(artist_data)

# Save the data
save_chart_data(clean_data,
                filename = paste0(make.names(ARTIST_NAME), "_chart_data"),
                format = "csv",
                data_dir = OUTPUT_DIR)

# 2. Basic Statistics
cat("\n2. Basic Artist Statistics:\n")

basic_stats <- clean_data %>%
  summarise(
    total_chart_entries = n(),
    unique_tracks = n_distinct(track_uri),
    chart_span_days = as.numeric(max(chart_date) - min(chart_date)) + 1,
    first_chart_date = min(chart_date),
    last_chart_date = max(chart_date),
    best_chart_position = min(chart_position, na.rm = TRUE),
    worst_chart_position = max(chart_position, na.rm = TRUE),
    avg_chart_position = round(mean(chart_position, na.rm = TRUE), 1),
    median_chart_position = median(chart_position, na.rm = TRUE),
    number_one_hits = sum(chart_position == 1, na.rm = TRUE),
    top_10_hits = sum(chart_position <= 10, na.rm = TRUE),
    top_50_hits = sum(chart_position <= 50, na.rm = TRUE)
  )

print(basic_stats)

# 3. Track Analysis
cat("\n3. Track Performance Analysis:\n")

track_performance <- clean_data %>%
  group_by(track_uri, track_name) %>%
  summarise(
    total_appearances = n(),
    best_position = min(chart_position, na.rm = TRUE),
    worst_position = max(chart_position, na.rm = TRUE),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    max_days_on_chart = max(days_on_chart, na.rm = TRUE),
    first_chart_date = min(chart_date),
    last_chart_date = max(chart_date),
    chart_span = as.numeric(max(chart_date) - min(chart_date)) + 1,
    .groups = "drop"
  ) %>%
  arrange(best_position, desc(total_appearances))

cat("\nTop 10 Tracks by Chart Position:\n")
print(track_performance %>%
      select(track_name, best_position, total_appearances, max_days_on_chart) %>%
      head(10))

cat("\nTracks with Most Chart Appearances:\n")
print(track_performance %>%
      arrange(desc(total_appearances)) %>%
      select(track_name, total_appearances, best_position, chart_span) %>%
      head(10))

# 4. Chart Position Trends
cat("\n4. Chart Position Analytics:\n")

position_analytics <- calculate_chart_positions(clean_data, artist_name = ARTIST_NAME)

if (nrow(position_analytics) > 0) {
  cat("\nPosition Analytics Summary:\n")
  print(position_analytics %>%
        select(track_name, peak_position, avg_position, total_days,
               position_improvement, chart_efficiency) %>%
        head(10))
}

# 5. Temporal Analysis
cat("\n5. Temporal Trends:\n")

# Monthly performance
monthly_performance <- clean_data %>%
  mutate(year_month = format(chart_date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(
    chart_entries = n(),
    unique_tracks = n_distinct(track_uri),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    best_position = min(chart_position, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_month)

cat("\nMonthly Performance:\n")
print(monthly_performance)

# Track trends over time
if (nrow(clean_data) > 0) {
  trends <- track_trends(clean_data, level = "artist", entity_name = ARTIST_NAME,
                        time_unit = "month", min_appearances = 1)

  if (nrow(trends) > 0) {
    cat("\nTrend Analysis:\n")
    print(trends %>%
          select(time_group, appearances, avg_position, best_position,
                 position_trend, momentum) %>%
          head(10))
  }
}

# 6. Chart Movement Analysis
cat("\n6. Chart Movement Analysis:\n")

movements <- clean_data %>%
  group_by(track_name, track_uri) %>%
  arrange(chart_date) %>%
  summarise(
    upward_moves = sum(position_change == "up", na.rm = TRUE),
    downward_moves = sum(position_change == "down", na.rm = TRUE),
    new_entries = sum(position_change == "new", na.rm = TRUE),
    biggest_jump = max(position_change_numeric, na.rm = TRUE),
    biggest_drop = min(position_change_numeric, na.rm = TRUE),
    net_movement = upward_moves - downward_moves,
    .groups = "drop"
  ) %>%
  filter(upward_moves + downward_moves > 0)

if (nrow(movements) > 0) {
  cat("\nBiggest Chart Jumps:\n")
  print(movements %>%
        arrange(desc(biggest_jump)) %>%
        select(track_name, biggest_jump, upward_moves, net_movement) %>%
        head(5))

  cat("\nBiggest Chart Drops:\n")
  print(movements %>%
        arrange(biggest_drop) %>%
        select(track_name, biggest_drop, downward_moves, net_movement) %>%
        head(5))
}

# 7. Performance Metrics
cat("\n7. Performance Metrics:\n")

# Calculate dominance score
dominance_data <- tibble(
  main_artist = ARTIST_NAME,
  chart_position = clean_data$chart_position,
  days_on_chart = clean_data$days_on_chart
)

dominance_metrics <- calculate_artist_dominance(dominance_data)

cat("\nChart Dominance Metrics:\n")
print(dominance_metrics %>%
      select(dominance_score, weighted_position, peak_achievement,
             sustained_success, total_appearances))

# 8. Generate Comprehensive Report
cat("\n8. Generating comprehensive report...\n")

artist_report <- list(
  artist_name = ARTIST_NAME,
  analysis_period = list(
    start_date = START_DATE,
    end_date = END_DATE,
    analysis_date = Sys.time()
  ),
  basic_statistics = basic_stats,
  track_performance = track_performance,
  position_analytics = if (nrow(position_analytics) > 0) position_analytics else NULL,
  temporal_trends = monthly_performance,
  movement_analysis = if (nrow(movements) > 0) movements else NULL,
  dominance_metrics = dominance_metrics,
  key_insights = list(
    best_performing_track = track_performance$track_name[1],
    most_appearances_track = track_performance %>%
      arrange(desc(total_appearances)) %>%
      slice(1) %>%
      pull(track_name),
    chart_consistency = round(basic_stats$total_chart_entries / basic_stats$chart_span_days, 3),
    peak_achievement = basic_stats$best_chart_position
  )
)

# Save the report
saveRDS(artist_report, file.path(OUTPUT_DIR, paste0(make.names(ARTIST_NAME), "_analysis.rds")))
jsonlite::write_json(artist_report, file.path(OUTPUT_DIR, paste0(make.names(ARTIST_NAME), "_analysis.json")),
                     pretty = TRUE, auto_unbox = TRUE)

# 9. Create Visualizations
if (requireNamespace("ggplot2", quietly = TRUE) && nrow(clean_data) > 0) {
  cat("\n9. Creating visualizations...\n")

  # Chart position over time
  if (nrow(clean_data) >= 2) {
    p1 <- clean_data %>%
      ggplot(aes(x = chart_date, y = chart_position)) +
      geom_line(alpha = 0.7, color = "steelblue") +
      geom_point(alpha = 0.7, color = "steelblue", size = 1) +
      scale_y_reverse() +  # Better positions at top
      labs(
        title = paste("Chart Positions Over Time -", ARTIST_NAME),
        x = "Date",
        y = "Chart Position (lower is better)",
        caption = "Note: Y-axis is reversed (1 = best position)"
      ) +
      theme_minimal()

    ggsave(file.path(OUTPUT_DIR, paste0(make.names(ARTIST_NAME), "_chart_positions.png")),
           plot = p1, width = 12, height = 8, dpi = 300)
  }

  # Track performance comparison
  if (nrow(track_performance) > 1) {
    top_tracks <- track_performance %>% head(10)

    p2 <- top_tracks %>%
      ggplot(aes(x = reorder(track_name, -best_position), y = best_position)) +
      geom_col(fill = "coral") +
      coord_flip() +
      scale_y_reverse() +
      labs(
        title = paste("Best Chart Positions by Track -", ARTIST_NAME),
        x = "Track",
        y = "Best Chart Position (lower is better)"
      ) +
      theme_minimal()

    ggsave(file.path(OUTPUT_DIR, paste0(make.names(ARTIST_NAME), "_track_comparison.png")),
           plot = p2, width = 12, height = 8, dpi = 300)
  }

  # Monthly activity
  if (nrow(monthly_performance) > 1) {
    p3 <- monthly_performance %>%
      mutate(date = as.Date(paste0(year_month, "-01"))) %>%
      ggplot(aes(x = date, y = chart_entries)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_point(color = "darkgreen", size = 2) +
      labs(
        title = paste("Monthly Chart Activity -", ARTIST_NAME),
        x = "Month",
        y = "Chart Entries"
      ) +
      theme_minimal() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(OUTPUT_DIR, paste0(make.names(ARTIST_NAME), "_monthly_activity.png")),
           plot = p3, width = 12, height = 6, dpi = 300)
  }
}

# 10. Summary
cat("\n=== Analysis Complete ===\n")
cat("Artist:", ARTIST_NAME, "\n")
cat("Total chart entries:", basic_stats$total_chart_entries, "\n")
cat("Unique tracks:", basic_stats$unique_tracks, "\n")
cat("Best chart position:", basic_stats$best_chart_position, "\n")
cat("Number one hits:", basic_stats$number_one_hits, "\n")
cat("Top 10 hits:", basic_stats$top_10_hits, "\n")
cat("\nResults saved to:", OUTPUT_DIR, "\n")

files_created <- list.files(OUTPUT_DIR)
if (length(files_created) > 0) {
  cat("Files generated:\n")
  cat(paste("  -", files_created), sep = "\n")
} else {
  cat("No files were generated.\n")
}

# Clean up
cleanup_package_environment(quiet = TRUE)

cat("\nArtist deep dive analysis completed!\n")
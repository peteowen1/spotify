#!/usr/bin/env Rscript

# Spotify Yearly Chart Analysis Script
# This script demonstrates how to analyze full year trends using the spotify package

# Load required packages
library(spotify)
library(dplyr)
library(ggplot2)
library(lubridate)

# Configuration
YEAR <- 2025
START_DATE <- paste0(YEAR, "-01-01")
END_DATE <- paste0(YEAR, "-12-31")
OUTPUT_DIR <- "yearly_analysis_output"

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("=== Spotify Yearly Analysis", YEAR, "===\n")
cat("Analysis period:", START_DATE, "to", END_DATE, "\n\n")

# Check internet connection
if (!check_internet()) {
  stop("Internet connection required for data scraping")
}

# Set up package environment
setup_package_environment(quiet = TRUE)

# Function to safely scrape with caching
safe_scrape_year <- function(year) {
  cache_key <- paste0("yearly_data_", year)

  # Try to load from cache first
  cached_data <- cache_data(cache_key)

  if (!is.null(cached_data)) {
    cat("Using cached data for", year, "\n")
    return(cached_data)
  }

  cat("Scraping data for", year, "...\n")
  cat("This may take a while for a full year of data.\n")

  # For demonstration, we'll scrape first week of each month
  # For a full year, you'd want to scrape all days
  sample_dates <- seq(
    from = as.Date(paste0(year, "-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "week"
  )[1:52]  # One date per week

  all_data <- tibble()

  for (i in seq_along(sample_dates)) {
    date <- sample_dates[i]
    cat("Scraping", as.character(date), "... (", i, "/", length(sample_dates), ")\n")

    daily_data <- get_daily_charts(
      date = date,
      chart_type = "regional-global-daily",
      region = "global",
      delay = 1
    )

    if (nrow(daily_data) > 0) {
      all_data <- bind_rows(all_data, daily_data)
    }
  }

  # Cache the scraped data
  cache_data(cache_key, all_data)

  return(all_data)
}

# 1. Data Collection
cat("1. Collecting chart data...\n")
yearly_data <- safe_scrape_year(YEAR)

if (nrow(yearly_data) == 0) {
  stop("No data collected. Check your internet connection or try different dates.")
}

cat("Collected", nrow(yearly_data), "chart entries\n")

# Clean the data
cat("2. Cleaning data...\n")
clean_data <- clean_spotify_data(yearly_data)

# Save raw data
save_chart_data(clean_data,
                filename = paste0("yearly_charts_", YEAR),
                format = "csv",
                data_dir = OUTPUT_DIR)

# 3. Top Artists Analysis
cat("3. Analyzing top artists...\n")

# Most appearances
top_appearances <- top_artists(clean_data, metric = "appearances", top_n = 20)
cat("\nTop Artists by Chart Appearances:\n")
print(top_appearances %>%
      select(main_artist, total_appearances, unique_tracks, best_position) %>%
      head(10))

# Best peak positions
top_peaks <- top_artists(clean_data, metric = "peak_positions", top_n = 20)
cat("\nTop Artists by Peak Positions:\n")
print(top_peaks %>%
      select(main_artist, best_peak_position, number_one_hits, top_10_hits) %>%
      head(10))

# Chart dominance
top_dominance <- top_artists(clean_data, metric = "chart_dominance", top_n = 20)
cat("\nTop Artists by Chart Dominance:\n")
print(top_dominance %>%
      select(main_artist, dominance_score, total_appearances, peak_achievement) %>%
      head(10))

# 4. Track Analysis
cat("4. Analyzing top tracks...\n")

# Longest chart runs
longest_runs <- clean_data %>%
  group_by(track_uri, track_name, main_artist) %>%
  summarise(
    max_days_on_chart = max(days_on_chart, na.rm = TRUE),
    total_appearances = n(),
    best_position = min(chart_position, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(max_days_on_chart)) %>%
  head(20)

cat("\nTracks with Longest Chart Runs:\n")
print(longest_runs %>% select(track_name, main_artist, max_days_on_chart, best_position) %>% head(10))

# Number one hits
number_ones <- clean_data %>%
  filter(chart_position == 1) %>%
  distinct(track_uri, .keep_all = TRUE) %>%
  select(track_name, main_artist, chart_date) %>%
  arrange(chart_date)

cat("\nNumber One Hits in", YEAR, ":\n")
if (nrow(number_ones) > 0) {
  print(number_ones)
} else {
  cat("No number one hits found in sample data\n")
}

# 5. Temporal Analysis
cat("5. Analyzing temporal trends...\n")

# Monthly chart activity
monthly_stats <- clean_data %>%
  mutate(month = floor_date(chart_date, "month")) %>%
  group_by(month) %>%
  summarise(
    entries = n(),
    unique_artists = n_distinct(main_artist),
    unique_tracks = n_distinct(track_uri),
    avg_position = mean(chart_position, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMonthly Chart Statistics:\n")
print(monthly_stats)

# 6. Generate Summary Report
cat("6. Generating summary report...\n")

summary_stats <- streaming_insights(clean_data, insight_type = "summary")

report <- list(
  analysis_info = list(
    year = YEAR,
    analysis_date = Sys.time(),
    total_entries = nrow(clean_data),
    date_range = range(clean_data$chart_date),
    package_version = get_package_version()
  ),
  top_artists = list(
    most_appearances = top_appearances$main_artist[1],
    best_peak = top_peaks$main_artist[1],
    most_dominant = top_dominance$main_artist[1]
  ),
  summary_statistics = summary_stats,
  temporal_trends = monthly_stats
)

# Save summary report
saveRDS(report, file.path(OUTPUT_DIR, paste0("yearly_summary_", YEAR, ".rds")))
jsonlite::write_json(report, file.path(OUTPUT_DIR, paste0("yearly_summary_", YEAR, ".json")),
                     pretty = TRUE, auto_unbox = TRUE)

# Create simple visualizations if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("7. Creating visualizations...\n")

  # Top artists chart
  p1 <- top_appearances %>%
    head(15) %>%
    ggplot(aes(x = reorder(main_artist, total_appearances), y = total_appearances)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = paste("Top Artists by Chart Appearances -", YEAR),
      x = "Artist",
      y = "Total Chart Appearances"
    ) +
    theme_minimal()

  ggsave(file.path(OUTPUT_DIR, paste0("top_artists_", YEAR, ".png")),
         plot = p1, width = 12, height = 8, dpi = 300)

  # Monthly activity chart
  p2 <- monthly_stats %>%
    ggplot(aes(x = month, y = entries)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(
      title = paste("Monthly Chart Activity -", YEAR),
      x = "Month",
      y = "Chart Entries"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month")

  ggsave(file.path(OUTPUT_DIR, paste0("monthly_activity_", YEAR, ".png")),
         plot = p2, width = 12, height = 6, dpi = 300)
}

cat("\n=== Analysis Complete ===\n")
cat("Results saved to:", OUTPUT_DIR, "\n")
cat("Files generated:\n")
list.files(OUTPUT_DIR) %>% paste("  -", .) %>% cat(sep = "\n")

# Answer the user's original question about total song streams
cat("\n=== ANSWER TO YOUR QUESTION ===\n")
cat("Artist with most total chart appearances (proxy for streams):\n")
cat("Artist:", top_appearances$main_artist[1], "\n")
cat("Total appearances:", top_appearances$total_appearances[1], "\n")
cat("Unique tracks:", top_appearances$unique_tracks[1], "\n")
cat("Best chart position:", top_appearances$best_position[1], "\n")

# Clean up
cleanup_package_environment(quiet = TRUE)

cat("\nAnalysis script completed successfully!\n")
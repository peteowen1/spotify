#!/usr/bin/env Rscript

# Spotify Chart Movements Analysis Script
# This script analyzes chart movements, jumps, drops, and position changes

# Load required packages
library(spotify)
library(dplyr)
library(ggplot2)
library(lubridate)

# Configuration
START_DATE <- "2025-09-01"
END_DATE <- "2025-09-15"  # Shorter range for movement analysis
OUTPUT_DIR <- "chart_movements_analysis"

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("=== Spotify Chart Movements Analysis ===\n")
cat("Analysis period:", START_DATE, "to", END_DATE, "\n\n")

# Check internet connection
if (!check_internet()) {
  stop("Internet connection required for data scraping")
}

# Set up package environment
setup_package_environment(quiet = TRUE)

# Function to scrape movement data
scrape_movement_data <- function(start_date, end_date) {
  cache_key <- paste0("movements_", gsub("-", "", start_date), "_", gsub("-", "", end_date))

  # Try cache first
  cached_data <- cache_data(cache_key)
  if (!is.null(cached_data)) {
    cat("Using cached movement data\n")
    return(cached_data)
  }

  cat("Scraping chart movement data...\n")

  # For movement analysis, we need consecutive days
  dates <- seq(from = as.Date(start_date),
               to = as.Date(end_date),
               by = "day")

  all_data <- tibble()

  for (i in seq_along(dates)) {
    cat("Scraping", as.character(dates[i]), "... (", i, "/", length(dates), ")\n")

    daily_data <- get_daily_charts(
      date = dates[i],
      chart_type = "regional-global-daily",
      region = "global",
      delay = 1.5  # Be extra respectful with consecutive requests
    )

    if (nrow(daily_data) > 0) {
      all_data <- bind_rows(all_data, daily_data)
    }
  }

  # Cache the results
  cache_data(cache_key, all_data)

  return(all_data)
}

# 1. Data Collection
cat("1. Collecting chart data...\n")
movement_data <- scrape_movement_data(START_DATE, END_DATE)

if (nrow(movement_data) == 0) {
  stop("No movement data collected. Check dates or connection.")
}

cat("Collected", nrow(movement_data), "chart entries\n")

# Clean the data
clean_data <- clean_spotify_data(movement_data)

# Save the data
save_chart_data(clean_data,
                filename = "chart_movements_data",
                format = "csv",
                data_dir = OUTPUT_DIR)

# 2. Basic Movement Statistics
cat("\n2. Movement Statistics Overview:\n")

movement_summary <- clean_data %>%
  group_by(position_change) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(clean_data) * 100, 1),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

print(movement_summary)

# Numeric movement analysis
numeric_movements <- clean_data %>%
  filter(!is.na(position_change_numeric), position_change_numeric != 0) %>%
  summarise(
    total_movements = n(),
    avg_movement = round(mean(position_change_numeric, na.rm = TRUE), 1),
    median_movement = median(position_change_numeric, na.rm = TRUE),
    biggest_jump = max(position_change_numeric, na.rm = TRUE),
    biggest_drop = min(position_change_numeric, na.rm = TRUE),
    total_upward = sum(position_change_numeric > 0, na.rm = TRUE),
    total_downward = sum(position_change_numeric < 0, na.rm = TRUE)
  )

cat("\nNumeric Movement Summary:\n")
print(numeric_movements)

# 3. Biggest Chart Jumps
cat("\n3. Biggest Chart Jumps:\n")

biggest_jumps <- clean_data %>%
  filter(position_change == "up", !is.na(position_change_numeric)) %>%
  arrange(desc(position_change_numeric)) %>%
  select(track_name, main_artist, chart_date, chart_position,
         previous_position, position_change_numeric, days_on_chart) %>%
  head(20)

print(biggest_jumps %>% head(10))

# 4. Biggest Chart Drops
cat("\n4. Biggest Chart Drops:\n")

biggest_drops <- clean_data %>%
  filter(position_change == "down", !is.na(position_change_numeric)) %>%
  arrange(position_change_numeric) %>%
  select(track_name, main_artist, chart_date, chart_position,
         previous_position, position_change_numeric, days_on_chart) %>%
  head(20)

print(biggest_drops %>% head(10))

# 5. New Entries Analysis
cat("\n5. New Chart Entries:\n")

new_entries <- clean_data %>%
  filter(position_change == "new") %>%
  arrange(chart_position) %>%
  select(track_name, main_artist, chart_date, chart_position, days_on_chart) %>%
  head(20)

cat("Total new entries:", nrow(clean_data %>% filter(position_change == "new")), "\n")

if (nrow(new_entries) > 0) {
  cat("\nBest new entries (by chart position):\n")
  print(new_entries %>% head(10))

  # Analyze debut positions
  debut_stats <- clean_data %>%
    filter(position_change == "new") %>%
    summarise(
      total_debuts = n(),
      avg_debut_position = round(mean(chart_position, na.rm = TRUE), 1),
      median_debut_position = median(chart_position, na.rm = TRUE),
      best_debut = min(chart_position, na.rm = TRUE),
      worst_debut = max(chart_position, na.rm = TRUE),
      debuts_top_10 = sum(chart_position <= 10, na.rm = TRUE),
      debuts_top_50 = sum(chart_position <= 50, na.rm = TRUE)
    )

  cat("\nDebut Position Statistics:\n")
  print(debut_stats)
}

# 6. Artist Movement Analysis
cat("\n6. Artist Movement Patterns:\n")

artist_movements <- clean_data %>%
  filter(position_change %in% c("up", "down")) %>%
  group_by(main_artist) %>%
  summarise(
    total_movements = n(),
    upward_moves = sum(position_change == "up", na.rm = TRUE),
    downward_moves = sum(position_change == "down", na.rm = TRUE),
    net_movement = upward_moves - downward_moves,
    avg_jump = round(mean(position_change_numeric[position_change == "up"], na.rm = TRUE), 1),
    avg_drop = round(mean(position_change_numeric[position_change == "down"], na.rm = TRUE), 1),
    biggest_single_jump = max(position_change_numeric, na.rm = TRUE),
    biggest_single_drop = min(position_change_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_movements >= 2) %>%
  arrange(desc(net_movement))

cat("\nArtists with Most Net Upward Movement:\n")
print(artist_movements %>%
      select(main_artist, total_movements, upward_moves, downward_moves, net_movement) %>%
      head(10))

cat("\nArtists with Biggest Single Jumps:\n")
print(artist_movements %>%
      arrange(desc(biggest_single_jump)) %>%
      select(main_artist, biggest_single_jump, avg_jump, upward_moves) %>%
      head(10))

# 7. Track Movement Patterns
cat("\n7. Track Movement Patterns:\n")

track_movements <- clean_data %>%
  group_by(track_uri, track_name, main_artist) %>%
  arrange(chart_date) %>%
  filter(n() > 1) %>%  # Only tracks with multiple chart appearances
  summarise(
    chart_days = n(),
    upward_moves = sum(position_change == "up", na.rm = TRUE),
    downward_moves = sum(position_change == "down", na.rm = TRUE),
    stable_days = sum(position_change == "same", na.rm = TRUE),
    movement_ratio = round(upward_moves / pmax(1, upward_moves + downward_moves), 2),
    biggest_jump = max(position_change_numeric, na.rm = TRUE),
    biggest_drop = min(position_change_numeric, na.rm = TRUE),
    net_movement = sum(position_change_numeric, na.rm = TRUE),
    volatility = sd(position_change_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(upward_moves + downward_moves > 0)

cat("\nMost Volatile Tracks (biggest position swings):\n")
print(track_movements %>%
      filter(!is.na(volatility)) %>%
      arrange(desc(volatility)) %>%
      select(track_name, main_artist, chart_days, volatility, biggest_jump, biggest_drop) %>%
      head(10))

cat("\nMost Consistent Climbers (high upward movement ratio):\n")
print(track_movements %>%
      filter(chart_days >= 3) %>%
      arrange(desc(movement_ratio), desc(upward_moves)) %>%
      select(track_name, main_artist, movement_ratio, upward_moves, net_movement) %>%
      head(10))

# 8. Position Range Analysis
cat("\n8. Chart Position Range Analysis:\n")

position_analysis <- clean_data %>%
  mutate(
    position_tier = case_when(
      chart_position <= 10 ~ "Top 10",
      chart_position <= 50 ~ "Top 50",
      chart_position <= 100 ~ "Top 100",
      TRUE ~ "Lower Chart"
    )
  ) %>%
  group_by(position_tier, position_change) %>%
  summarise(
    count = n(),
    avg_movement = round(mean(position_change_numeric, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  tidyr::spread(position_change, count, fill = 0)

print(position_analysis)

# 9. Daily Movement Trends
cat("\n9. Daily Movement Trends:\n")

daily_movements <- clean_data %>%
  group_by(chart_date) %>%
  summarise(
    total_entries = n(),
    new_entries = sum(position_change == "new", na.rm = TRUE),
    upward_moves = sum(position_change == "up", na.rm = TRUE),
    downward_moves = sum(position_change == "down", na.rm = TRUE),
    stable_positions = sum(position_change == "same", na.rm = TRUE),
    avg_movement = round(mean(position_change_numeric, na.rm = TRUE), 1),
    movement_activity = upward_moves + downward_moves,
    .groups = "drop"
  ) %>%
  arrange(chart_date)

print(daily_movements)

# 10. Generate Movement Report
cat("\n10. Generating movement analysis report...\n")

movement_report <- list(
  analysis_period = list(
    start_date = START_DATE,
    end_date = END_DATE,
    analysis_date = Sys.time()
  ),
  summary_statistics = list(
    total_entries = nrow(clean_data),
    movement_summary = movement_summary,
    numeric_movements = numeric_movements,
    debut_stats = if (exists("debut_stats")) debut_stats else NULL
  ),
  top_movements = list(
    biggest_jumps = biggest_jumps,
    biggest_drops = biggest_drops,
    best_new_entries = new_entries
  ),
  artist_analysis = artist_movements,
  track_analysis = track_movements,
  daily_trends = daily_movements,
  insights = list(
    most_volatile_period = daily_movements %>% arrange(desc(movement_activity)) %>% slice(1),
    most_stable_period = daily_movements %>% arrange(movement_activity) %>% slice(1),
    biggest_single_jump = clean_data %>%
      filter(!is.na(position_change_numeric)) %>%
      arrange(desc(position_change_numeric)) %>%
      slice(1) %>%
      select(track_name, main_artist, position_change_numeric),
    biggest_single_drop = clean_data %>%
      filter(!is.na(position_change_numeric)) %>%
      arrange(position_change_numeric) %>%
      slice(1) %>%
      select(track_name, main_artist, position_change_numeric)
  )
)

# Save the report
saveRDS(movement_report, file.path(OUTPUT_DIR, "chart_movements_report.rds"))
jsonlite::write_json(movement_report, file.path(OUTPUT_DIR, "chart_movements_report.json"),
                     pretty = TRUE, auto_unbox = TRUE)

# 11. Create Visualizations
if (requireNamespace("ggplot2", quietly = TRUE) && nrow(clean_data) > 0) {
  cat("\n11. Creating movement visualizations...\n")

  # Movement distribution
  p1 <- movement_summary %>%
    filter(position_change != "unknown") %>%
    ggplot(aes(x = reorder(position_change, count), y = count)) +
    geom_col(fill = c("red", "green", "blue", "orange")[1:nrow(.)]) +
    coord_flip() +
    labs(
      title = "Distribution of Chart Movements",
      x = "Movement Type",
      y = "Number of Occurrences"
    ) +
    theme_minimal()

  ggsave(file.path(OUTPUT_DIR, "movement_distribution.png"),
         plot = p1, width = 10, height = 6, dpi = 300)

  # Daily movement activity
  if (nrow(daily_movements) > 1) {
    p2 <- daily_movements %>%
      ggplot(aes(x = as.Date(chart_date))) +
      geom_line(aes(y = upward_moves, color = "Upward"), size = 1) +
      geom_line(aes(y = downward_moves, color = "Downward"), size = 1) +
      geom_line(aes(y = new_entries, color = "New Entries"), size = 1) +
      labs(
        title = "Daily Chart Movement Activity",
        x = "Date",
        y = "Number of Movements",
        color = "Movement Type"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Upward" = "green", "Downward" = "red", "New Entries" = "blue"))

    ggsave(file.path(OUTPUT_DIR, "daily_movement_activity.png"),
           plot = p2, width = 12, height = 6, dpi = 300)
  }

  # Movement magnitude distribution
  if (nrow(clean_data %>% filter(!is.na(position_change_numeric), position_change_numeric != 0)) > 0) {
    p3 <- clean_data %>%
      filter(!is.na(position_change_numeric), position_change_numeric != 0) %>%
      ggplot(aes(x = position_change_numeric)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Distribution of Chart Movement Magnitudes",
        x = "Position Change (positive = jump up, negative = drop down)",
        y = "Frequency"
      ) +
      theme_minimal()

    ggsave(file.path(OUTPUT_DIR, "movement_magnitude_distribution.png"),
           plot = p3, width = 12, height = 6, dpi = 300)
  }
}

# 12. Summary
cat("\n=== Chart Movement Analysis Complete ===\n")
cat("Analysis period:", START_DATE, "to", END_DATE, "\n")
cat("Total chart entries analyzed:", nrow(clean_data), "\n")
cat("Total movements (up/down):", sum(movement_summary$count[movement_summary$position_change %in% c("up", "down")]), "\n")

if (nrow(biggest_jumps) > 0) {
  cat("Biggest single jump:",
      biggest_jumps$position_change_numeric[1], "positions by",
      biggest_jumps$track_name[1], "by", biggest_jumps$main_artist[1], "\n")
}

if (nrow(biggest_drops) > 0) {
  cat("Biggest single drop:",
      abs(biggest_drops$position_change_numeric[1]), "positions by",
      biggest_drops$track_name[1], "by", biggest_drops$main_artist[1], "\n")
}

cat("\nResults saved to:", OUTPUT_DIR, "\n")

files_created <- list.files(OUTPUT_DIR)
if (length(files_created) > 0) {
  cat("Files generated:\n")
  cat(paste("  -", files_created), sep = "\n")
}

# Clean up
cleanup_package_environment(quiet = TRUE)

cat("\nChart movement analysis completed!\n")
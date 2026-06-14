#!/usr/bin/env Rscript

# Spotify Regional Chart Comparison Script
# This script compares chart performance across different regions

# Load required packages
library(spotify)
library(dplyr)
library(ggplot2)
library(lubridate)

# Configuration
TARGET_DATE <- "2025-09-01"  # Single date for regional comparison
REGIONS <- c("global", "US", "GB", "DE", "FR", "ES", "AU", "CA")  # Regions to compare
OUTPUT_DIR <- "regional_comparison_analysis"

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("=== Spotify Regional Chart Comparison ===\n")
cat("Analysis date:", TARGET_DATE, "\n")
cat("Regions:", paste(REGIONS, collapse = ", "), "\n\n")

# Check internet connection
if (!check_internet()) {
  stop("Internet connection required for data scraping")
}

# Set up package environment
setup_package_environment(quiet = TRUE)

# Function to scrape regional data
scrape_regional_data <- function(date, regions) {
  cache_key <- paste0("regional_", gsub("-", "", date), "_", paste(regions, collapse = "_"))

  # Try cache first
  cached_data <- cache_data(cache_key)
  if (!is.null(cached_data)) {
    cat("Using cached regional data\n")
    return(cached_data)
  }

  cat("Scraping regional chart data for", length(regions), "regions...\n")

  all_data <- tibble()

  for (region in regions) {
    cat("Scraping", region, "charts...\n")

    regional_data <- get_daily_charts(
      date = date,
      chart_type = "regional-global-daily",
      region = region,
      delay = 2  # Be extra respectful with regional requests
    )

    if (nrow(regional_data) > 0) {
      all_data <- bind_rows(all_data, regional_data)
    } else {
      cat("No data found for region:", region, "\n")
    }
  }

  # Cache the results
  cache_data(cache_key, all_data)

  return(all_data)
}

# 1. Data Collection
cat("1. Collecting regional chart data...\n")
regional_data <- scrape_regional_data(TARGET_DATE, REGIONS)

if (nrow(regional_data) == 0) {
  stop("No regional data collected. Check date or regions.")
}

cat("Collected", nrow(regional_data), "chart entries across regions\n")

# Clean the data
clean_data <- clean_spotify_data(regional_data)

# Filter for regions we actually have data for
available_regions <- unique(clean_data$region)
cat("Available regions in data:", paste(available_regions, collapse = ", "), "\n")

# Save the data
save_chart_data(clean_data,
                filename = paste0("regional_comparison_", gsub("-", "", TARGET_DATE)),
                format = "csv",
                data_dir = OUTPUT_DIR)

# 2. Regional Overview
cat("\n2. Regional Chart Overview:\n")

regional_summary <- clean_data %>%
  group_by(region) %>%
  summarise(
    total_entries = n(),
    unique_artists = n_distinct(main_artist),
    unique_tracks = n_distinct(track_uri),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    median_position = median(chart_position, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_entries))

print(regional_summary)

# 3. Cross-Regional Artists
cat("\n3. Artists Appearing in Multiple Regions:\n")

cross_regional_artists <- clean_data %>%
  group_by(main_artist) %>%
  summarise(
    regions_count = n_distinct(region),
    regions = paste(sort(unique(region)), collapse = ", "),
    total_appearances = n(),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    best_position = min(chart_position, na.rm = TRUE),
    position_variance = round(var(chart_position, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(regions_count > 1) %>%
  arrange(desc(regions_count), desc(total_appearances))

cat("Artists appearing in", max(cross_regional_artists$regions_count, na.rm = TRUE), "or more regions:\n")
print(cross_regional_artists %>%
      select(main_artist, regions_count, total_appearances, avg_position, best_position) %>%
      head(15))

# 4. Cross-Regional Tracks
cat("\n4. Tracks Appearing in Multiple Regions:\n")

cross_regional_tracks <- clean_data %>%
  group_by(track_uri, track_name, main_artist) %>%
  summarise(
    regions_count = n_distinct(region),
    regions = paste(sort(unique(region)), collapse = ", "),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    best_position = min(chart_position, na.rm = TRUE),
    worst_position = max(chart_position, na.rm = TRUE),
    position_range = worst_position - best_position,
    position_variance = round(var(chart_position, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(regions_count > 1) %>%
  arrange(desc(regions_count), best_position)

cat("Top tracks appearing in multiple regions:\n")
print(cross_regional_tracks %>%
      select(track_name, main_artist, regions_count, best_position,
             position_range, position_variance) %>%
      head(15))

# 5. Regional Preferences Analysis
cat("\n5. Regional Preferences:\n")

# Artists unique to specific regions
regional_exclusives <- clean_data %>%
  group_by(main_artist) %>%
  filter(n_distinct(region) == 1) %>%
  group_by(region, main_artist) %>%
  summarise(
    appearances = n(),
    best_position = min(chart_position, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(region) %>%
  arrange(best_position, desc(appearances)) %>%
  slice_head(n = 5) %>%
  ungroup()

cat("\nTop region-exclusive artists (appearing in only one region):\n")
for (region in available_regions) {
  region_artists <- regional_exclusives %>%
    filter(region == !!region) %>%
    select(main_artist, best_position, appearances)

  if (nrow(region_artists) > 0) {
    cat("\n", region, "exclusives:\n")
    print(region_artists)
  }
}

# 6. Position Comparison Analysis
cat("\n6. Cross-Regional Position Comparison:\n")

# For tracks that appear in multiple regions, compare their positions
position_comparison <- clean_data %>%
  group_by(track_uri, track_name, main_artist) %>%
  filter(n() > 1) %>%  # Only tracks in multiple regions
  select(track_name, main_artist, region, chart_position) %>%
  tidyr::pivot_wider(names_from = region, values_from = chart_position, names_prefix = "pos_") %>%
  ungroup()

if (nrow(position_comparison) > 0) {
  cat("Tracks with biggest position differences between regions:\n")

  # Calculate position variance for each track across regions
  position_stats <- clean_data %>%
    group_by(track_uri, track_name, main_artist) %>%
    filter(n() > 1) %>%
    summarise(
      regions_count = n(),
      best_position = min(chart_position),
      worst_position = max(chart_position),
      position_difference = worst_position - best_position,
      position_variance = var(chart_position),
      .groups = "drop"
    ) %>%
    arrange(desc(position_difference))

  print(position_stats %>%
        select(track_name, main_artist, regions_count, best_position,
               worst_position, position_difference) %>%
        head(10))
}

# 7. Regional Market Size Analysis
cat("\n7. Regional Market Analysis:\n")

market_analysis <- clean_data %>%
  group_by(region) %>%
  summarise(
    chart_size = max(chart_position, na.rm = TRUE),
    top_10_competition = n_distinct(main_artist[chart_position <= 10]),
    top_50_competition = n_distinct(main_artist[chart_position <= 50]),
    artist_diversity = n_distinct(main_artist) / n(),
    avg_days_on_chart = round(mean(days_on_chart, na.rm = TRUE), 1),
    .groups = "drop"
  )

print(market_analysis)

# 8. Genre/Artist Style Patterns (Simplified Analysis)
cat("\n8. Regional Artist Patterns:\n")

# Analyze which artists dominate in which regions
artist_dominance <- clean_data %>%
  group_by(region, main_artist) %>%
  summarise(
    appearances = n(),
    avg_position = round(mean(chart_position, na.rm = TRUE), 1),
    dominance_score = sum(1 / chart_position, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(region) %>%
  arrange(desc(dominance_score)) %>%
  slice_head(n = 5) %>%
  ungroup()

cat("\nTop artists by region (based on chart dominance):\n")
for (region in available_regions) {
  region_top <- artist_dominance %>%
    filter(region == !!region) %>%
    select(main_artist, appearances, avg_position, dominance_score)

  if (nrow(region_top) > 0) {
    cat("\n", region, "top artists:\n")
    print(region_top)
  }
}

# 9. Similarity Analysis Between Regions
cat("\n9. Regional Similarity Analysis:\n")

# Calculate Jaccard similarity between regions based on shared artists
if (length(available_regions) > 1) {
  similarity_matrix <- matrix(0,
                              nrow = length(available_regions),
                              ncol = length(available_regions),
                              dimnames = list(available_regions, available_regions))

  for (i in 1:length(available_regions)) {
    for (j in 1:length(available_regions)) {
      region1_artists <- clean_data %>%
        filter(region == available_regions[i]) %>%
        pull(main_artist) %>%
        unique()

      region2_artists <- clean_data %>%
        filter(region == available_regions[j]) %>%
        pull(main_artist) %>%
        unique()

      intersection <- length(intersect(region1_artists, region2_artists))
      union <- length(union(region1_artists, region2_artists))

      similarity_matrix[i, j] <- if (union > 0) round(intersection / union, 3) else 0
    }
  }

  cat("\nJaccard similarity matrix (based on shared artists):\n")
  print(similarity_matrix)

  # Find most and least similar regions
  upper_tri <- similarity_matrix
  upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA

  max_similarity <- max(upper_tri, na.rm = TRUE)
  min_similarity <- min(upper_tri, na.rm = TRUE)

  most_similar <- which(upper_tri == max_similarity, arr.ind = TRUE)
  least_similar <- which(upper_tri == min_similarity, arr.ind = TRUE)

  if (nrow(most_similar) > 0) {
    cat("\nMost similar regions:")
    cat(available_regions[most_similar[1, 1]], "and", available_regions[most_similar[1, 2]], "(", max_similarity, ")\n")
  }

  if (nrow(least_similar) > 0) {
    cat("Least similar regions:")
    cat(available_regions[least_similar[1, 1]], "and", available_regions[least_similar[1, 2]], "(", min_similarity, ")\n")
  }
}

# 10. Generate Regional Comparison Report
cat("\n10. Generating regional comparison report...\n")

regional_report <- list(
  analysis_date = TARGET_DATE,
  regions_analyzed = available_regions,
  analysis_timestamp = Sys.time(),
  regional_summary = regional_summary,
  cross_regional_analysis = list(
    artists = cross_regional_artists,
    tracks = cross_regional_tracks
  ),
  regional_exclusives = regional_exclusives,
  position_comparison = if (exists("position_stats")) position_stats else NULL,
  market_analysis = market_analysis,
  artist_dominance = artist_dominance,
  similarity_analysis = if (exists("similarity_matrix")) similarity_matrix else NULL,
  key_insights = list(
    most_global_artist = if (nrow(cross_regional_artists) > 0)
      cross_regional_artists$main_artist[1] else NULL,
    most_global_track = if (nrow(cross_regional_tracks) > 0)
      paste(cross_regional_tracks$track_name[1], "by", cross_regional_tracks$main_artist[1]) else NULL,
    largest_market = regional_summary$region[which.max(regional_summary$total_entries)],
    most_diverse_market = market_analysis$region[which.max(market_analysis$artist_diversity)]
  )
)

# Save the report
saveRDS(regional_report, file.path(OUTPUT_DIR, "regional_comparison_report.rds"))
jsonlite::write_json(regional_report, file.path(OUTPUT_DIR, "regional_comparison_report.json"),
                     pretty = TRUE, auto_unbox = TRUE)

# 11. Create Visualizations
if (requireNamespace("ggplot2", quietly = TRUE) && nrow(clean_data) > 0) {
  cat("\n11. Creating regional visualizations...\n")

  # Regional chart sizes comparison
  p1 <- regional_summary %>%
    ggplot(aes(x = reorder(region, total_entries), y = total_entries)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Chart Entries by Region",
      x = "Region",
      y = "Number of Chart Entries"
    ) +
    theme_minimal()

  ggsave(file.path(OUTPUT_DIR, "regional_chart_sizes.png"),
         plot = p1, width = 10, height = 6, dpi = 300)

  # Artist diversity by region
  p2 <- market_analysis %>%
    ggplot(aes(x = reorder(region, artist_diversity), y = artist_diversity)) +
    geom_col(fill = "coral") +
    coord_flip() +
    labs(
      title = "Artist Diversity by Region",
      x = "Region",
      y = "Artist Diversity Score (unique artists / total entries)"
    ) +
    theme_minimal()

  ggsave(file.path(OUTPUT_DIR, "regional_artist_diversity.png"),
         plot = p2, width = 10, height = 6, dpi = 300)

  # Cross-regional artist distribution
  if (nrow(cross_regional_artists) > 0) {
    p3 <- cross_regional_artists %>%
      count(regions_count, name = "artist_count") %>%
      ggplot(aes(x = factor(regions_count), y = artist_count)) +
      geom_col(fill = "darkgreen") +
      labs(
        title = "Distribution of Artists Across Regions",
        x = "Number of Regions",
        y = "Number of Artists"
      ) +
      theme_minimal()

    ggsave(file.path(OUTPUT_DIR, "cross_regional_distribution.png"),
           plot = p3, width = 10, height = 6, dpi = 300)
  }

  # Position comparison for cross-regional tracks
  if (exists("position_stats") && nrow(position_stats) > 0) {
    p4 <- position_stats %>%
      filter(regions_count >= 2) %>%
      head(20) %>%
      ggplot(aes(x = reorder(paste(track_name, "-", main_artist), position_difference),
                 y = position_difference)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(
        title = "Tracks with Biggest Regional Position Differences",
        x = "Track - Artist",
        y = "Position Difference (worst - best)"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))

    ggsave(file.path(OUTPUT_DIR, "position_differences.png"),
           plot = p4, width = 12, height = 10, dpi = 300)
  }

  # Heatmap of similarity matrix
  if (exists("similarity_matrix") && length(available_regions) > 2) {
    library(reshape2)  # For melt function
    similarity_df <- melt(similarity_matrix, varnames = c("Region1", "Region2"), value.name = "Similarity")

    p5 <- similarity_df %>%
      ggplot(aes(x = Region1, y = Region2, fill = Similarity)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(
        title = "Regional Similarity Matrix",
        subtitle = "Based on shared artists (Jaccard similarity)",
        x = "Region",
        y = "Region"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(OUTPUT_DIR, "similarity_heatmap.png"),
           plot = p5, width = 8, height = 6, dpi = 300)
  }
}

# 12. Summary
cat("\n=== Regional Comparison Analysis Complete ===\n")
cat("Analysis date:", TARGET_DATE, "\n")
cat("Regions analyzed:", length(available_regions), "(", paste(available_regions, collapse = ", "), ")\n")
cat("Total chart entries:", nrow(clean_data), "\n")

if (nrow(cross_regional_artists) > 0) {
  cat("Most global artist:", cross_regional_artists$main_artist[1],
      "(", cross_regional_artists$regions_count[1], "regions )\n")
}

if (nrow(cross_regional_tracks) > 0) {
  cat("Most global track:", cross_regional_tracks$track_name[1], "by", cross_regional_tracks$main_artist[1],
      "(", cross_regional_tracks$regions_count[1], "regions )\n")
}

cat("Largest market:", regional_summary$region[which.max(regional_summary$total_entries)],
    "(", max(regional_summary$total_entries), "entries )\n")

cat("\nResults saved to:", OUTPUT_DIR, "\n")

files_created <- list.files(OUTPUT_DIR)
if (length(files_created) > 0) {
  cat("Files generated:\n")
  cat(paste("  -", files_created), sep = "\n")
}

# Clean up
cleanup_package_environment(quiet = TRUE)

cat("\nRegional comparison analysis completed!\n")
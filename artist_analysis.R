library(tidyverse)
# Load your package
devtools::load_all()

# Get chart data with streams
charts <- get_daily_charts("2025-08-01")

# Check if streams data is now included
head(charts[, c("chart_position", "track_name", "main_artist", "streams", "streams_formatted")])

# Find all artists (including features/collaborators) and their total streams
all_artist_streams <- get_all_artists_streams(charts, top_n = 20)
print(all_artist_streams)

# Compare main artists vs all artists rankings
comparison <- compare_main_vs_all_artists(charts)
print(comparison$main_artists)
print(comparison$all_artists)

charts

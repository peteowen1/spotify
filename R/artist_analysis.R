#' Get All Artists Stream Totals
#'
#' Analyzes the all_artists column to get total streams for every artist,
#' including featured artists, collaborators, etc.
#'
#' @param data Chart data tibble with all_artists and streams columns
#' @param min_streams Minimum total streams to include artist (default: 0)
#' @param top_n Number of top artists to return (default: 50)
#'
#' @return Tibble with all artists ranked by total streams
#' @export
#'
#' @examples
#' \dontrun{
#' charts <- get_daily_charts("2025-09-01")
#' all_artist_streams <- get_all_artists_streams(charts, top_n = 20)
#' print(all_artist_streams)
#' }
get_all_artists_streams <- function(data, min_streams = 0, top_n = 50) {

  if (nrow(data) == 0) {
    return(tibble::tibble())
  }

  # Check required columns
  required_cols <- c("all_artists", "streams", "track_name")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(glue::glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  # Split artists and create long format data
  artist_streams <- data %>%
    # Remove rows with missing data
    dplyr::filter(!is.na(.data$all_artists), !is.na(.data$streams)) %>%

    # Convert streams to numeric if needed
    dplyr::mutate(
      streams_numeric = dplyr::case_when(
        is.character(.data$streams) ~ as.numeric(gsub(",", "", .data$streams)),
        TRUE ~ as.numeric(.data$streams)
      )
    ) %>%

    # Split the comma-separated artists
    dplyr::mutate(
      # Clean up the artist string and split
      artist_list = stringr::str_split(.data$all_artists, ",")
    ) %>%

    # Expand to one row per artist
    tidyr::unnest(.data$artist_list) %>%

    # Clean artist names
    dplyr::mutate(
      artist_clean = stringr::str_trim(.data$artist_list)
    ) %>%

    # Remove empty names
    dplyr::filter(.data$artist_clean != "") %>%

    # Group by artist and sum streams
    dplyr::group_by(.data$artist_clean) %>%
    dplyr::summarise(
      total_streams = sum(.data$streams_numeric, na.rm = TRUE),
      total_songs = dplyr::n(),
      unique_songs = dplyr::n_distinct(.data$track_name),
      avg_streams_per_song = round(mean(.data$streams_numeric, na.rm = TRUE)),
      best_chart_position = min(.data$chart_position, na.rm = TRUE),
      .groups = "drop"
    ) %>%

    # Filter by minimum streams
    dplyr::filter(.data$total_streams >= min_streams) %>%

    # Sort by total streams
    dplyr::arrange(dplyr::desc(.data$total_streams)) %>%

    # Add ranking
    dplyr::mutate(rank = dplyr::row_number()) %>%

    # Select top N
    dplyr::slice_head(n = top_n) %>%

    # Reorder columns
    dplyr::select(.data$rank, artist_name = .data$artist_clean, .data$total_streams,
                  .data$total_songs, .data$unique_songs, .data$avg_streams_per_song,
                  .data$best_chart_position)

  return(artist_streams)
}

#' Compare Main vs All Artists
#'
#' Compares stream totals when counting only main artists vs all artists
#'
#' @param data Chart data tibble
#' @param top_n Number of top artists to show in comparison (default: 20)
#'
#' @return List with main_artists and all_artists rankings
#' @export
#'
#' @examples
#' \dontrun{
#' charts <- get_daily_charts("2025-09-01")
#' comparison <- compare_main_vs_all_artists(charts)
#'
#' # See who gains/loses when including features
#' print(comparison$main_artists)
#' print(comparison$all_artists)
#' }
compare_main_vs_all_artists <- function(data, top_n = 20) {

  # Main artists only
  main_artists <- data %>%
    dplyr::filter(!is.na(.data$streams)) %>%
    dplyr::mutate(
      streams_numeric = dplyr::case_when(
        is.character(.data$streams) ~ as.numeric(gsub(",", "", .data$streams)),
        TRUE ~ as.numeric(.data$streams)
      )
    ) %>%
    dplyr::group_by(.data$main_artist) %>%
    dplyr::summarise(
      total_streams = sum(.data$streams_numeric, na.rm = TRUE),
      total_songs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_streams)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(.data$rank, artist_name = .data$main_artist, .data$total_streams, .data$total_songs)

  # All artists (including features)
  all_artists <- get_all_artists_streams(data, top_n = top_n) %>%
    dplyr::select(.data$rank, .data$artist_name, .data$total_streams, .data$total_songs)

  return(list(
    main_artists = main_artists,
    all_artists = all_artists,
    summary = list(
      analysis_date = Sys.time(),
      total_chart_entries = nrow(data),
      unique_main_artists = length(unique(data$main_artist)),
      estimated_total_artists = nrow(get_all_artists_streams(data, top_n = 1000))
    )
  ))
}

#' Find Artist Collaborations
#'
#' Finds all collaborations for a specific artist
#'
#' @param data Chart data tibble
#' @param artist_name Name of the artist to analyze
#' @param min_streams Minimum streams for collaboration to be included (default: 100000)
#'
#' @return Tibble with collaboration details
#' @export
#'
#' @examples
#' \dontrun{
#' charts <- get_daily_charts("2025-09-01")
#' bad_bunny_collabs <- find_artist_collaborations(charts, "Bad Bunny")
#' print(bad_bunny_collabs)
#' }
find_artist_collaborations <- function(data, artist_name, min_streams = 100000) {

  collaborations <- data %>%
    # Convert streams to numeric first
    dplyr::mutate(
      streams_numeric = dplyr::case_when(
        is.character(.data$streams) ~ as.numeric(gsub(",", "", .data$streams)),
        TRUE ~ as.numeric(.data$streams)
      )
    ) %>%
    # Find songs where artist appears in all_artists
    dplyr::filter(
      stringr::str_detect(.data$all_artists, stringr::fixed(artist_name, ignore_case = TRUE)),
      !is.na(.data$streams_numeric),
      .data$streams_numeric >= min_streams
    ) %>%

    # Extract collaborators
    dplyr::mutate(
      # Split artists and find collaborators (not including the target artist)
      artist_list = stringr::str_split(.data$all_artists, ","),
      collaborators = purrr::map_chr(.data$artist_list, function(artists) {
        clean_artists <- stringr::str_trim(artists)
        # Remove the target artist and combine remaining
        other_artists <- clean_artists[!stringr::str_detect(clean_artists, stringr::fixed(artist_name, ignore_case = TRUE))]
        if (length(other_artists) > 0) {
          paste(other_artists, collapse = ", ")
        } else {
          "Solo"
        }
      }),
      is_main_artist = .data$main_artist == artist_name
    ) %>%

    dplyr::select(.data$track_name, .data$main_artist, .data$collaborators,
                  streams = .data$streams_numeric, .data$chart_position, .data$is_main_artist) %>%

    dplyr::arrange(dplyr::desc(.data$streams))

  return(collaborations)
}

#' Get Featured Artist Stats
#'
#' Analyzes artists who appear mostly as features vs main artists
#'
#' @param data Chart data tibble
#' @param min_appearances Minimum total appearances to include (default: 2)
#'
#' @return Tibble with feature vs main artist statistics
#' @export
#'
#' @examples
#' \dontrun{
#' charts <- get_daily_charts("2025-09-01")
#' feature_stats <- get_featured_artist_stats(charts)
#' print(feature_stats)
#' }
get_featured_artist_stats <- function(data, min_appearances = 2) {

  # Get all artist appearances
  all_appearances <- data %>%
    dplyr::filter(!is.na(.data$all_artists), !is.na(.data$streams)) %>%
    dplyr::mutate(
      streams_numeric = dplyr::case_when(
        is.character(.data$streams) ~ as.numeric(gsub(",", "", .data$streams)),
        TRUE ~ as.numeric(.data$streams)
      ),
      artist_list = stringr::str_split(.data$all_artists, ",")
    ) %>%
    tidyr::unnest(.data$artist_list) %>%
    dplyr::mutate(
      artist_clean = stringr::str_trim(.data$artist_list),
      is_main = .data$artist_clean == .data$main_artist
    ) %>%
    dplyr::filter(.data$artist_clean != "")

  # Calculate stats per artist
  feature_stats <- all_appearances %>%
    dplyr::group_by(.data$artist_clean) %>%
    dplyr::summarise(
      total_appearances = dplyr::n(),
      main_artist_appearances = sum(.data$is_main),
      featured_appearances = sum(!.data$is_main),
      total_streams = sum(.data$streams_numeric, na.rm = TRUE),
      streams_as_main = sum(.data$streams_numeric[.data$is_main], na.rm = TRUE),
      streams_as_feature = sum(.data$streams_numeric[!.data$is_main], na.rm = TRUE),
      .groups = "drop"
    ) %>%

    # Filter by minimum appearances
    dplyr::filter(.data$total_appearances >= min_appearances) %>%

    # Calculate percentages
    dplyr::mutate(
      pct_featured = round(.data$featured_appearances / .data$total_appearances * 100, 1),
      pct_streams_from_features = round(.data$streams_as_feature / .data$total_streams * 100, 1),
      artist_type = dplyr::case_when(
        .data$pct_featured >= 80 ~ "Mostly Featured",
        .data$pct_featured >= 50 ~ "Mixed",
        .data$pct_featured >= 20 ~ "Mostly Main",
        TRUE ~ "Solo Artist"
      )
    ) %>%

    dplyr::arrange(dplyr::desc(.data$total_streams)) %>%
    dplyr::select(.data$artist_clean, .data$total_streams, .data$total_appearances,
                  .data$main_artist_appearances, .data$featured_appearances,
                  .data$pct_featured, .data$streams_as_main, .data$streams_as_feature,
                  .data$pct_streams_from_features, .data$artist_type)

  return(feature_stats)
}
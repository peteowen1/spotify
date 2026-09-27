# CLAUDE.md — spotify

R package for scraping and analyzing Spotify Charts data via Spotify's own charts endpoints. Sibling to [`kworb/`](../kworb/) (kworb.net) — spotify focuses on regional/daily/viral snapshots with direct-from-Spotify data, kworb is better for per-track historical depth.

## Development Commands

```r
devtools::load_all()    # Iterative dev
devtools::document()    # Regenerate NAMESPACE + man/ (never edit NAMESPACE manually)
```

(No `tests/` directory; no `devtools::check()` workflow set up.)

## Pipeline

```
get_spotify_token()           → OAuth (cached in package env)
  → get_daily_charts(date, chart_type, region)
    → parse_chart_response() → tibble of track × position × streams
  → clean_spotify_data()     → normalisation
  → save_chart_data() / cache_data()
  → aggregate_streams() / chart_performance() / streaming_insights() / top_artists() / track_trends()
  → get_all_artists_streams() / get_featured_artist_stats() / find_artist_collaborations()
```

37 exports across 8 R files.

## Module Map

| File | Purpose |
|------|---------|
| `authentication.R` | OAuth — `get_spotify_token()`, `set_spotify_token()`, `clear_spotify_token()`, `has_spotify_token()`, `test_spotify_auth()`, `refresh_token_help()` |
| `scrapers.R` | `get_daily_charts()`, `get_regional_charts()`, `get_chart_range()`, `parse_chart_response()` |
| `data_processing.R` | `clean_spotify_data()`, `validate_data_integrity()`, `validate_dates()`, `calculate_chart_positions()` |
| `data_storage.R` | `save_chart_data()`, `load_chart_data()`, `cache_data()`, `clear_cache()`, `list_saved_data()`, `create_data_backup()`, `restore_data_backup()` |
| `analysis.R` | `aggregate_streams()`, `chart_performance()`, `streaming_insights()`, `top_artists()`, `track_trends()` |
| `artist_analysis.R` | `get_all_artists_streams()`, `get_featured_artist_stats()`, `find_artist_collaborations()`, `compare_main_vs_all_artists()` |
| `utils.R` | `check_internet()`, `format_numbers()`, `create_progress_bar()`, `rate_limit()`, `get_package_version()`, env setup/cleanup |
| `globals.R` | NSE declarations for `R CMD check` |

## Chart Types

`get_daily_charts(chart_type = ...)` accepts:
- `"regional-global-daily"` (default), `"regional-global-weekly"` — official chart
- `"viral-global-daily"`, `"viral-global-weekly"` — viral chart

Combine with `region` for country-specific cuts (`get_regional_charts()` is the targeted helper).

## Key Conventions

- **Auth is package-env scoped** — `get_spotify_token()` caches a token in `.pkg_env` (set up in `zzz.R`-style init via `setup_package_environment()`). `clear_spotify_token()` to force re-auth.
- **Token failures are recoverable** — `test_spotify_auth()` runs a sanity probe before bulk scrapes; `refresh_token_help()` prints the OAuth dance steps when a refresh hangs. Full OAuth setup walkthrough lives in `inst/AUTHENTICATION.md`.
- **Polite scraping** — `rate_limit()` throttles requests; `get_chart_range(start, end)` paces multi-day pulls automatically.
- **Prefer parquet/CSV over RDS** for `save_chart_data()` (global rule from `~/.claude/CLAUDE.md`). The defaults already do this — don't change them.
- **Artist analysis: "main" vs "all"** — Spotify attributes streams to the first-billed artist by default. `get_all_artists_streams()` redistributes to every credited artist; `compare_main_vs_all_artists()` quantifies the gap. Use the "all" view when measuring collaboration value.

## Cache & Backup

- `cache_data()` / `clear_cache()` — in-memory + disk cache for scraped chart pages
- `create_data_backup()` / `restore_data_backup()` — snapshot the saved store before destructive ops
- `list_saved_data()` / `get_chart_range()` — inspect coverage before re-scraping
- `validate_data_integrity()` — sanity check after merges (no duplicate position×date×track combos, no out-of-range streams)

# Spotify Charts API Authentication

The Spotify Charts API requires authentication via a Bearer token. This guide explains how to obtain and use the authentication token.

## Quick Start

1. **Get your token from the browser:**
   ```r
   # See detailed instructions
   get_token_instructions()
   ```

2. **Set your token:**
   ```r
   set_spotify_token("your_long_token_string_here")
   ```

3. **Test authentication:**
   ```r
   test_spotify_auth()
   ```

4. **Start scraping:**
   ```r
   charts <- get_daily_charts("2025-09-01")
   ```

## Detailed Steps

### Step 1: Obtain Token from Browser

1. Go to [https://charts.spotify.com](https://charts.spotify.com) in your web browser
2. Open Developer Tools:
   - **Chrome/Edge**: Press F12 or right-click → "Inspect"
   - **Firefox**: Press F12 or right-click → "Inspect Element"
3. Go to the **Network** tab in Developer Tools
4. Refresh the page or navigate to any chart
5. Look for requests to `charts-spotify-com-service.spotify.com`
6. Click on one of these requests
7. In the **Request Headers** section, find the `authorization` header
8. Copy the long token string after `Bearer ` (don't include "Bearer ")

### Step 2: Set Token in R

```r
library(spotify)

# Set your token (replace with your actual token)
set_spotify_token("BQABdSeALLUM_QGfzs2B8XujcJP58CgHzbXJeZJ0EPEEAMZUPDYshJJDqktIxwmL4...")

# Verify it works
if (test_spotify_auth()) {
  message("Ready to scrape!")
} else {
  message("Token not working - try getting a fresh one")
}
```

### Step 3: Use the Package

Once authenticated, all package functions work normally:

```r
# Get daily charts
daily_charts <- get_daily_charts("2025-09-01")

# Get data for a date range
weekly_charts <- get_chart_range("2025-09-01", "2025-09-07")

# Get regional charts
regional_charts <- get_regional_charts("2025-09-01", regions = c("US", "GB", "DE"))

# Analyze top artists
top_artists_2025 <- top_artists(daily_charts, metric = "appearances")
```

## Token Management

### Check if token is set:
```r
has_spotify_token()
```

### Get current token:
```r
token <- get_spotify_token()
```

### Clear token:
```r
clear_spotify_token()
```

## Important Notes

- **Tokens expire**: Browser tokens typically last for the duration of your browser session
- **Refresh when needed**: If you get 401 Unauthorized errors, get a fresh token
- **Don't share tokens**: Tokens are tied to your Spotify account
- **Rate limiting**: The package includes built-in rate limiting to be respectful to Spotify's servers

## Troubleshooting

### "No Spotify token available" error:
- Make sure you've called `set_spotify_token()` with your token

### "Authentication failed" errors:
- Get a fresh token from your browser
- Make sure you copied the complete token string
- Don't include "Bearer " in the token string

### "HTTP 401 Unauthorized" errors:
- Your token has expired - get a new one
- Test with `test_spotify_auth()`

### No data returned:
- Check that the date exists and has chart data
- Some older dates may not have data available
- Try a recent date first to test your setup

## Example Complete Workflow

```r
library(spotify)

# 1. Set up authentication
set_spotify_token("your_token_here")
test_spotify_auth()

# 2. Scrape some data
charts <- get_daily_charts("2025-09-01")
clean_data <- clean_spotify_data(charts)

# 3. Find top artists
top_artists <- top_artists(clean_data, metric = "appearances", top_n = 10)
print(top_artists)

# 4. Save data
save_chart_data(clean_data, "daily_charts_sep_01", format = "csv")
```
#' Set Spotify Authentication Token
#'
#' Sets the Bearer token required for Spotify Charts API access.
#' You can obtain this token from your browser's developer tools when visiting charts.spotify.com
#'
#' @param token Character string containing the Bearer token (without "Bearer " prefix)
#' @param store_globally Logical, whether to store the token in global environment (default: TRUE)
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Set your token (get this from browser developer tools)
#' set_spotify_token("BQABdSeALLUM_QGfzs2B...")
#'
#' # Now you can make API calls
#' charts <- get_daily_charts("2025-09-01")
#' }
set_spotify_token <- function(token, store_globally = TRUE) {
  if (is.null(token) || token == "" || !is.character(token)) {
    stop("Token must be a valid character string")
  }

  # Remove "Bearer " prefix if user included it
  token <- sub("^Bearer\\s+", "", token)

  if (store_globally) {
    # Store in global environment
    assign("spotify_auth_token", token, envir = .GlobalEnv)
    message("Spotify token stored globally")
  } else {
    # Store in package environment
    assign("spotify_auth_token", token, envir = asNamespace("spotify"))
    message("Spotify token stored in package environment")
  }

  return(invisible(TRUE))
}

#' Get Spotify Authentication Token
#'
#' Retrieves the stored Spotify authentication token
#'
#' @return Character string with the token, or NULL if not set
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_spotify_token()
#' if (is.null(token)) {
#'   message("No token set. Use set_spotify_token() first.")
#' }
#' }
get_spotify_token <- function() {
  # Check if we have a stored token
  if (exists("spotify_auth_token", envir = .GlobalEnv)) {
    token <- get("spotify_auth_token", envir = .GlobalEnv)
    if (!is.null(token) && token != "") {
      return(token)
    }
  }

  # If no token or interactive session, prompt for one
  if (interactive()) {
    cat("\n=== SPOTIFY TOKEN REQUIRED ===\n")
    cat("1. Go to: https://charts.spotify.com\n")
    cat("2. Press F12 (Developer Tools) → Network tab\n")
    cat("3. Refresh the page\n")
    cat("4. Find request to 'charts-spotify-com-service.spotify.com'\n")
    cat("5. Copy the Authorization token (the long string after 'Bearer ')\n")
    cat("6. Paste it below:\n\n")

    token <- readline("Enter your Spotify token: ")
    token <- stringr::str_trim(token)

    # Remove "Bearer " if user included it
    token <- sub("^Bearer\\s+", "", token)

    if (token != "" && nchar(token) > 50) {
      # Store the token
      assign("spotify_auth_token", token, envir = .GlobalEnv)
      cat("✓ Token saved! Continuing with scraping...\n\n")
      return(token)
    } else {
      stop("Invalid token provided. Please try again.")
    }
  } else {
    stop("No Spotify token available. In interactive mode, you'll be prompted to enter one.")
  }
}

#' Check if Spotify Token is Available
#'
#' Checks whether a Spotify authentication token has been set
#'
#' @return Logical, TRUE if token is available
#' @export
#'
#' @examples
#' \dontrun{
#' if (!has_spotify_token()) {
#'   stop("Please set your Spotify token first with set_spotify_token()")
#' }
#' }
has_spotify_token <- function() {
  token <- get_spotify_token()
  return(!is.null(token) && token != "")
}

#' Clear Spotify Authentication Token
#'
#' Removes the stored Spotify authentication token
#'
#' @return Invisible TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' clear_spotify_token()
#' }
clear_spotify_token <- function() {
  # Remove from global environment
  if (exists("spotify_auth_token", envir = .GlobalEnv)) {
    rm("spotify_auth_token", envir = .GlobalEnv)
  }

  # Remove from package environment
  if (exists("spotify_auth_token", envir = asNamespace("spotify"))) {
    rm("spotify_auth_token", envir = asNamespace("spotify"))
  }

  message("Spotify token cleared")
  return(invisible(TRUE))
}

# Internal helper to clear token
clear_token <- function() {
  if (exists("spotify_auth_token", envir = .GlobalEnv)) {
    rm("spotify_auth_token", envir = .GlobalEnv)
  }
}


#' Test Spotify Authentication
#'
#' Tests whether the current authentication token works
#'
#' @param date Date to test with (default: today's date)
#'
#' @return Logical, TRUE if authentication works
#' @export
#'
#' @examples
#' \dontrun{
#' if (test_spotify_auth()) {
#'   message("Authentication successful!")
#' } else {
#'   message("Authentication failed. Check your token.")
#' }
#' }
test_spotify_auth <- function(date = Sys.Date()) {
  if (!has_spotify_token()) {
    message("No token available. Use set_spotify_token() first.")
    return(FALSE)
  }

  # Test with a simple request
  tryCatch({
    formatted_date <- format(as.Date(date), "%Y-%m-%d")
    url <- glue::glue("https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-global-daily/{formatted_date}")

    response <- make_authenticated_spotify_request(url, retries = 1)

    if (!is.null(response)) {
      message("Authentication test successful!")
      return(TRUE)
    } else {
      message("Authentication test failed - no response received")
      return(FALSE)
    }

  }, error = function(e) {
    if (grepl("invalid_token|401", e$message, ignore.case = TRUE)) {
      message("Token has expired! Get a fresh token from your browser.")
      message("Visit: https://charts.spotify.com")
      message("Then use: set_spotify_token('new_token_here')")
    } else {
      message(glue::glue("Authentication test failed: {e$message}"))
    }
    return(FALSE)
  })
}

#' Quick Token Refresh Helper
#'
#' Provides quick instructions for refreshing an expired token
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # When you get authentication errors:
#' refresh_token_help()
#' }
refresh_token_help <- function() {
  cat("\n=== TOKEN EXPIRED - QUICK REFRESH GUIDE ===\n")
  cat("1. Go to: https://charts.spotify.com\n")
  cat("2. Press F12 (Developer Tools)\n")
  cat("3. Click Network tab\n")
  cat("4. Refresh the page\n")
  cat("5. Find request to 'charts-spotify-com-service.spotify.com'\n")
  cat("6. Copy the authorization token (after 'Bearer ')\n")
  cat("7. Run: set_spotify_token('your_new_token')\n")
  cat("8. Test: test_spotify_auth()\n")
  cat("===============================================\n\n")

  return(invisible(NULL))
}

#' Make Authenticated Spotify Request
#'
#' Internal function to make HTTP requests with authentication
#'
#' @param url URL to request
#' @param retries Number of retry attempts
#' @param delay Seconds to wait between retries
#'
#' @return Parsed JSON response or NULL if failed
#' @keywords internal
make_authenticated_spotify_request <- function(url, retries = 3, delay = 1) {

  # This will prompt for token if none exists
  token <- get_spotify_token()

  if (is.null(token) || token == "") {
    stop("No Spotify token available.")
  }

  for (attempt in 1:retries) {
    tryCatch({
      # Make request with authentication
      response <- httr2::request(url) %>%
        httr2::req_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
          "Accept" = "application/json",
          "Authorization" = paste("Bearer", token),
          "Origin" = "https://charts.spotify.com",
          "Referer" = "https://charts.spotify.com/",
          "app-platform" = "Browser",
          "spotify-app-version" = "0.0.0.production"
        ) %>%
        httr2::req_perform()

      # Check if successful
      if (httr2::resp_status(response) == 200) {
        return(httr2::resp_body_json(response))
      } else if (httr2::resp_status(response) == 401) {
        # Token is expired - clear it and try to get a new one
        if (attempt == 1 && interactive()) {
          cat("Token expired! Getting fresh token...\n")
          clear_token()
          token <- get_spotify_token()  # This will prompt for new token
          # Continue to next attempt with new token
          next
        } else {
          warning(glue::glue("Authentication failed on attempt {attempt}. Token may be expired."))
        }
      } else {
        warning(glue::glue("Request failed with status {httr2::resp_status(response)} on attempt {attempt}"))
      }

    }, error = function(e) {
      if (grepl("401|Unauthorized", e$message) && attempt == 1 && interactive()) {
        # Token issue - clear and get new one
        cat("Token issue detected! Getting fresh token...\n")
        clear_token()
        token <- get_spotify_token()  # This will prompt for new token
        # Continue to next attempt with new token
        next
      } else {
        warning(glue::glue("Request error on attempt {attempt}: {e$message}"))
      }
    })

    # Wait before retry (except on last attempt)
    if (attempt < retries && delay > 0) {
      Sys.sleep(delay)
    }
  }

  return(NULL)
}

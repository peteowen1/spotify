# Global variables and imports for package

# Suppress R CMD check NOTEs about global variables
utils::globalVariables(c(
  "chart_span_days",
  "size_bytes",
  "spotify_auth_token",
  "."
))

#' @importFrom dplyr %>% mutate filter select group_by summarise arrange
#' @importFrom dplyr n n_distinct bind_rows case_when distinct slice_head
#' @importFrom dplyr across all_of desc pull slice rename count
#' @importFrom rlang .data
#' @importFrom stats lm median sd setNames var
#' @importFrom utils head tail txtProgressBar setTxtProgressBar packageVersion
#' @importFrom utils modifyList zip unzip
#' @importFrom glue glue
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate year month floor_date
#' @importFrom stringr str_trim str_squish str_detect
#' @importFrom jsonlite toJSON fromJSON write_json read_json
#' @importFrom httr2 request req_headers req_perform resp_status resp_body_json
#' @importFrom httr2 req_method req_timeout
#' @importFrom readr write_csv read_csv
#' @importFrom purrr map_dfr map_chr
NULL
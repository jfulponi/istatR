#' @title ISTAT API Base Functions
#' @description Core functions for communicating with the ISTAT SDMX REST API
#' @name base
NULL

# API Configuration
# Note: The API base URL changed from the old sdmx.istat.it to the new esploradati.istat.it
.istat_config <- new.env(parent = emptyenv())
.istat_config$base_url <- "https://esploradati.istat.it/SDMXWS/rest"
.istat_config$agency_id <- "IT1"
.istat_config$timeout <- 300  # Default timeout in seconds (5 minutes)

#' Get ISTAT API base URL
#'
#' @return Character string with the base URL
#' @keywords internal
get_base_url <- function() {
  .istat_config$base_url
}

#' Get ISTAT agency ID
#'
#' @return Character string with the agency ID
#' @keywords internal
get_agency_id <- function() {
  .istat_config$agency_id
}

#' Get or set the API timeout
#'
#' The ISTAT API can be slow to respond, especially for large queries.
#' This function allows you to get or set the timeout value in seconds.
#' The default timeout is 300 seconds (5 minutes).
#'
#' @param seconds Optional. If provided, sets the timeout to this value in seconds.
#'   If NULL (default), returns the current timeout value.
#'
#' @return If \code{seconds} is NULL, returns the current timeout value.
#'   If \code{seconds} is provided, invisibly returns the previous timeout value.
#'
#' @export
#'
#' @examples
#' # Get current timeout
#' istat_timeout()
#'
#' # Set timeout to 10 minutes
#' istat_timeout(600)
#'
#' # Set timeout back to default
#' istat_timeout(300)
istat_timeout <- function(seconds = NULL) {
  if (is.null(seconds)) {
    return(.istat_config$timeout)
  }
  old_timeout <- .istat_config$timeout

  .istat_config$timeout <- as.numeric(seconds)
  invisible(old_timeout)
}

#' Make a request to the ISTAT API
#'
#' @param path API endpoint path (without base URL)
#' @param accept Accept header value (default: application/xml)
#' @param ... Additional query parameters
#'
#' @return httr2 response object
#' @keywords internal
#'
#' @importFrom httr2 request req_headers req_user_agent req_retry req_perform req_timeout
istat_request <- function(path, accept = "application/xml", ...) {
  url <- paste0(get_base_url(), "/", path)

  # Build query parameters
  query_params <- list(...)

  # Build URL with query parameters
  if (length(query_params) > 0) {
    query_string <- paste(
      names(query_params),
      query_params,
      sep = "=",
      collapse = "&"
    )
    url <- paste0(url, "?", query_string)
  }

  req <- httr2::request(url) |>
    httr2::req_headers(Accept = accept) |>
    httr2::req_user_agent("istatR R package (https://github.com/your-username/istatR)") |>
    httr2::req_timeout(.istat_config$timeout) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 0.5 * 2^.x)

  httr2::req_perform(req)
}

#' Make a request and return XML with namespaces
#'
#' @param path API endpoint path
#' @param ... Additional query parameters
#'
#' @return A list with 'xml' (the xml document) and 'ns' (the namespaces)
#' @keywords internal
#'
#' @importFrom httr2 resp_body_xml
#' @importFrom xml2 xml_ns
istat_request_xml <- function(path, ...) {
  resp <- istat_request(path, accept = "application/xml", ...)
  xml <- httr2::resp_body_xml(resp)
  ns <- xml2::xml_ns(xml)
  list(xml = xml, ns = ns)
}

#' Make a request and return CSV as tibble
#'
#' @param path API endpoint path
#' @param ... Additional query parameters
#'
#' @return tibble with the data
#' @keywords internal
#'
#' @importFrom httr2 resp_body_string
#' @importFrom readr read_csv
istat_request_csv <- function(path, ...) {
  resp <- istat_request(path, accept = "text/csv", ...)
  csv_text <- httr2::resp_body_string(resp)
  readr::read_csv(csv_text, show_col_types = FALSE)
}

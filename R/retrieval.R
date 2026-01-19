#' @title ISTAT Data Retrieval Functions
#' @description Functions for retrieving data from ISTAT datasets
#' @name retrieval
NULL

#' Retrieve data from an ISTAT dataset
#'
#' Fetches data from an ISTAT dataset using the currently set filters.
#' The data is returned as a tibble with the TIME_PERIOD column converted to
#' Date format and sorted in ascending order.
#'
#' @param dataset An istat_dataset object with filters set
#' @param start_period Optional start date for filtering (format: YYYY-MM-DD or YYYY)
#' @param end_period Optional end date for filtering (format: YYYY-MM-DD or YYYY)
#' @param last_n_observations Optional integer to get only the last N observations
#'
#' @return A tibble containing the requested data with columns including:
#'   \describe{
#'     \item{DATAFLOW}{Dataset identifier}
#'     \item{FREQ}{Frequency}
#'     \item{TIME_PERIOD}{Time period (as Date)}
#'     \item{OBS_VALUE}{Observation value}
#'     \item{...}{Additional dimension and metadata columns}
#'   }
#'
#' @export
#'
#' @importFrom dplyr arrange mutate
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Create and configure dataset
#' ds <- istat_dataset("139_176")
#' ds <- set_filters(ds,
#'   FREQ = "M",
#'   TIPO_DATO = c("ISAV", "ESAV"),
#'   PAESE_PARTNER = "WORLD"
#' )
#'
#' # Get all data
#' data <- get_data(ds)
#'
#' # Get data for a specific time range
#' data <- get_data(ds, start_period = "2020-01-01", end_period = "2023-12-31")
#'
#' # Get only the last 12 observations
#' data <- get_data(ds, last_n_observations = 12)
#' }
get_data <- function(dataset,
                     start_period = NULL,
                     end_period = NULL,
                     last_n_observations = NULL) {

  # Build the URL key from filters
  url_key <- make_url_key(dataset$filters)

  # Build the path
  path <- paste0("data/", dataset$df_id)
  if (nchar(url_key) > 0) {
    path <- paste0(path, "/", url_key)
  }

  # Add query parameters
  query_params <- list()
  if (!is.null(start_period)) {
    query_params$startPeriod <- start_period
  }
  if (!is.null(end_period)) {
    query_params$endPeriod <- end_period
  }
  if (!is.null(last_n_observations)) {
    query_params$lastNObservations <- last_n_observations
  }

  # Make request
  data <- do.call(istat_request_csv, c(list(path = path), query_params))

  # Process TIME_PERIOD column if it exists
  if ("TIME_PERIOD" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      TIME_PERIOD = parse_time_period(.data$TIME_PERIOD)
    )

    # Sort by TIME_PERIOD ascending
    data <- dplyr::arrange(data, .data$TIME_PERIOD)
  }

  data
}

#' Parse ISTAT time period strings to Date
#'
#' Handles various SDMX time period formats including:
#' - Annual: YYYY
#' - Monthly: YYYY-MM
#' - Quarterly: YYYY-Q1, YYYY-Q2, etc.
#' - Weekly: YYYY-W01, etc.
#' - Daily: YYYY-MM-DD
#'
#' @param x Character vector of time period strings
#'
#' @return Date vector
#' @keywords internal
parse_time_period <- function(x) {
  x <- as.character(x)

  # Parse each time period string
  parsed <- vapply(x, function(tp) {
    tp <- as.character(tp)

    # Annual format: YYYY
    if (grepl("^\\d{4}$", tp)) {
      return(as.numeric(as.Date(paste0(tp, "-01-01"))))
    }

    # Monthly format: YYYY-MM
    if (grepl("^\\d{4}-\\d{2}$", tp)) {
      return(as.numeric(as.Date(paste0(tp, "-01"))))
    }

    # Quarterly format: YYYY-Q1, YYYY-Q2, YYYY-Q3, YYYY-Q4
    if (grepl("^\\d{4}-Q[1-4]$", tp)) {
      year <- substr(tp, 1, 4)
      quarter <- as.integer(substr(tp, 7, 7))
      month <- (quarter - 1) * 3 + 1
      return(as.numeric(as.Date(sprintf("%s-%02d-01", year, month))))
    }

    # Semester format: YYYY-S1, YYYY-S2
    if (grepl("^\\d{4}-S[1-2]$", tp)) {
      year <- substr(tp, 1, 4)
      semester <- as.integer(substr(tp, 7, 7))
      month <- (semester - 1) * 6 + 1
      return(as.numeric(as.Date(sprintf("%s-%02d-01", year, month))))
    }

    # Weekly format: YYYY-W01 to YYYY-W53
    if (grepl("^\\d{4}-W\\d{2}$", tp)) {
      year <- as.integer(substr(tp, 1, 4))
      week <- as.integer(substr(tp, 7, 8))
      # Approximate: return first day of year + weeks
      return(as.numeric(as.Date(paste0(year, "-01-01")) + (week - 1) * 7))
    }

    # Try standard date parsing (YYYY-MM-DD)
    tryCatch(
      as.numeric(as.Date(tp)),
      error = function(e) NA_real_
    )
  }, numeric(1), USE.NAMES = FALSE)

  as.Date(parsed, origin = "1970-01-01")
}

#' Quick data retrieval
#'
#' A convenience function that combines creating a dataset, setting filters,
#' and retrieving data in one call.
#'
#' @param dataflow_id Dataflow ID (e.g., "139_176")
#' @param ... Named filter arguments (dimension_id = value)
#' @param start_period Optional start date
#' @param end_period Optional end date
#' @param last_n_observations Optional integer to get only the last N observations
#'
#' @return A tibble containing the requested data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick retrieval with filters
#' data <- istat_get(
#'   "139_176",
#'   FREQ = "M",
#'   TIPO_DATO = "ISAV",
#'   PAESE_PARTNER = "WORLD",
#'   start_period = "2020-01-01"
#' )
#' }
istat_get <- function(dataflow_id,
                      ...,
                      start_period = NULL,
                      end_period = NULL,
                      last_n_observations = NULL) {

  # Create dataset

ds <- istat_dataset(dataflow_id)

  # Set filters
  filters <- list(...)
  if (length(filters) > 0) {
    ds <- do.call(set_filters, c(list(dataset = ds), filters))
  }

  # Get data
  get_data(
    ds,
    start_period = start_period,
    end_period = end_period,
    last_n_observations = last_n_observations
  )
}

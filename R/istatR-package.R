#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' istatR: R Interface to the Italian National Institute of Statistics (ISTAT) API
#'
#' The istatR package provides an R interface to the ISTAT SDMX RESTful API,
#' allowing users to discover available datasets, explore their structure and
#' dimensions, and retrieve statistical data from the Italian National Institute
#' of Statistics.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{all_available}}}{List all available ISTAT datasets}
#'   \item{\code{\link{search_dataset}}}{Search datasets by keyword}
#'   \item{\code{\link{istat_dataset}}}{Create a dataset object for exploration}
#'   \item{\code{\link{get_data}}}{Retrieve data from a dataset}
#' }
#'
#' @section Typical Workflow:
#' \preformatted{
#' library(istatR)
#'
#' # 1. Explore available datasets
#' datasets <- all_available()
#' import_ds <- search_dataset("import")
#'
#' # 2. Create a dataset object
#' ds <- istat_dataset("139_176")
#'
#' # 3. Explore dimensions
#' dimensions_info(ds)
#' get_dimension_values(ds, "TIPO_DATO")
#'
#' # 4. Set filters
#' ds <- set_filters(ds,
#'   FREQ = "M",
#'   TIPO_DATO = c("ISAV", "ESAV"),
#'   PAESE_PARTNER = "WORLD"
#' )
#'
#' # 5. Retrieve data
#' data <- get_data(ds)
#' }
#'
#' @docType package
#' @name istatR-package
NULL

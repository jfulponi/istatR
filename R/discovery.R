#' @title ISTAT Dataset Discovery Functions
#' @description Functions for discovering and exploring ISTAT datasets
#' @name discovery
NULL

#' List all available ISTAT datasets
#'
#' Retrieves a list of all available datasets (dataflows) from the ISTAT API.
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{df_id}{Dataflow ID}
#'     \item{version}{Dataset version}
#'     \item{df_description}{English description of the dataset}
#'     \item{df_structure_id}{Data structure definition ID}
#'   }
#'
#' @export
#'
#' @importFrom xml2 xml_find_all xml_attr xml_text xml_find_first
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#' # Get all available datasets
#' datasets <- all_available()
#' head(datasets)
#' }
all_available <- function() {
  path <- paste0("dataflow/", get_agency_id())
  result <- istat_request_xml(path)
  xml <- result$xml
  ns <- result$ns

  # Use namespace-aware XPath
  dataflows <- xml2::xml_find_all(xml, ".//structure:Dataflow", ns)

  purrr::map_df(dataflows, function(df) {
    # Get dataflow attributes
    df_id <- xml2::xml_attr(df, "id")
    version <- xml2::xml_attr(df, "version")

    # Get English description
    df_description <- get_name_by_lang_ns(df, "en", ns)

    # Get structure reference
    structure_ref <- xml2::xml_find_first(df, ".//structure:Structure/Ref", ns)
    df_structure_id <- if (!is.na(structure_ref)) {
      xml2::xml_attr(structure_ref, "id")
    } else {
      NA_character_
    }

    tibble::tibble(
      df_id = df_id,
      version = version,
      df_description = df_description,
      df_structure_id = df_structure_id
    )
  })
}

#' Search for datasets by keyword
#'
#' Searches available ISTAT datasets by keyword in their description.
#' The search is case-insensitive.
#'
#' @param keyword Character string to search for in dataset descriptions
#'
#' @return A tibble with matching datasets (same columns as \code{all_available()})
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_to_lower
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Search for datasets related to imports
#' import_datasets <- search_dataset("import")
#'
#' # Search for population datasets
#' pop_datasets <- search_dataset("population")
#' }
search_dataset <- function(keyword) {
  datasets <- all_available()

  results <- dplyr::filter(
    datasets,
    stringr::str_detect(
      stringr::str_to_lower(.data$df_description),
      stringr::str_to_lower(keyword)
    )
  )

  if (nrow(results) == 0) {
    warning("No datasets found matching keyword: ", keyword)
  }

  results
}

#' Create an ISTAT dataset object
#'
#' Creates a dataset object for a specific ISTAT dataflow. This object can be used
#' to explore the dataset's structure, dimensions, and available values, and to
#' set filters before retrieving data.
#'
#' @param dataflow_identifier Either a dataflow ID (e.g., "139_176"),
#'   a structure ID, or an exact dataset description
#'
#' @return A list with class "istat_dataset" containing:
#'   \describe{
#'     \item{df_id}{Dataflow ID}
#'     \item{version}{Dataset version}
#'     \item{df_description}{Dataset description}
#'     \item{df_structure_id}{Data structure definition ID}
#'     \item{dimensions}{Named list of dimension information}
#'     \item{filters}{Named list of current filters (initialized to "." for all)}
#'   }
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Create dataset by ID
#' ds <- istat_dataset("139_176")
#'
#' # View dimensions
#' dimensions_info(ds)
#'
#' # Get available values for a dimension
#' get_dimension_values(ds, "TIPO_DATO")
#'
#' # Set filters
#' ds <- set_filters(ds, FREQ = "M", TIPO_DATO = c("ISAV", "ESAV"))
#' }
istat_dataset <- function(dataflow_identifier) {
  # Get all available datasets
  all_ds <- all_available()

  # Try to match the identifier
  match_row <- NULL

  # First try exact df_id match
  idx <- which(all_ds$df_id == dataflow_identifier)
  if (length(idx) > 0) {
    match_row <- all_ds[idx[1], ]
  }

  # Try structure_id match
  if (is.null(match_row)) {
    idx <- which(all_ds$df_structure_id == dataflow_identifier)
    if (length(idx) > 0) {
      match_row <- all_ds[idx[1], ]
    }
  }

  # Try exact description match
  if (is.null(match_row)) {
    idx <- which(all_ds$df_description == dataflow_identifier)
    if (length(idx) > 0) {
      match_row <- all_ds[idx[1], ]
    }
  }

  if (is.null(match_row)) {
    stop("Could not find dataset with identifier: ", dataflow_identifier)
  }

  # Get dimensions
  dimensions <- get_dimensions(match_row$df_structure_id)

  # Initialize filters to "." (all values) for each dimension
  filters <- setNames(
    as.list(rep(".", length(dimensions))),
    names(dimensions)
  )

  structure(
    list(
      df_id = match_row$df_id,
      version = match_row$version,
      df_description = match_row$df_description,
      df_structure_id = match_row$df_structure_id,
      dimensions = dimensions,
      filters = filters
    ),
    class = "istat_dataset"
  )
}

#' Print method for istat_dataset
#'
#' @param x An istat_dataset object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input
#' @export
print.istat_dataset <- function(x, ...) {
  cat("ISTAT Dataset\n")
  cat("-------------\n")
  cat("ID:          ", x$df_id, "\n")
  cat("Version:     ", x$version, "\n")
  cat("Description: ", x$df_description, "\n")
  cat("Structure:   ", x$df_structure_id, "\n")
  cat("\nDimensions (", length(x$dimensions), "):\n", sep = "")
  for (dim_id in names(x$dimensions)) {
    filter_val <- x$filters[[dim_id]]
    filter_str <- if (identical(filter_val, ".")) {
      "(all)"
    } else if (length(filter_val) > 1) {
      paste0("[", paste(filter_val, collapse = ", "), "]")
    } else {
      filter_val
    }
    cat("  - ", dim_id, ": ", filter_str, "\n", sep = "")
  }
  invisible(x)
}

#' Get dimensions for a data structure
#'
#' @param structure_id Data structure definition ID
#'
#' @return Named list of dimensions with their metadata
#' @keywords internal
#'
#' @importFrom xml2 xml_find_all xml_attr
#' @importFrom purrr map
get_dimensions <- function(structure_id) {
  path <- paste0("datastructure/", get_agency_id(), "/", structure_id)
  result <- istat_request_xml(path)
  xml <- result$xml
  ns <- result$ns

  # Use namespace-aware XPath for Dimension elements
  dim_nodes <- xml2::xml_find_all(xml, ".//structure:Dimension", ns)

  dims <- list()
  for (dim_node in dim_nodes) {
    dim_id <- xml2::xml_attr(dim_node, "id")
    position <- xml2::xml_attr(dim_node, "position")

    # Skip dimensions without a valid ID (e.g., TIME_PERIOD dimension descriptor)
    if (is.na(dim_id) || dim_id == "") {
      next
    }

    # Get local representation reference for codelist
    local_rep <- xml2::xml_find_first(dim_node, ".//structure:LocalRepresentation//Ref", ns)
    codelist_id <- if (!is.na(local_rep)) {
      xml2::xml_attr(local_rep, "id")
    } else {
      NA_character_
    }

    dims[[dim_id]] <- list(
      id = dim_id,
      position = as.integer(position),
      codelist_id = codelist_id
    )
  }

  # Sort by position (remove NAs first)
  valid_dims <- dims[!is.na(sapply(dims, function(d) d$position))]
  valid_dims <- valid_dims[order(sapply(valid_dims, function(d) d$position))]
  valid_dims
}

#' Get information about dataset dimensions
#'
#' Returns information about the dimensions of a dataset, including their
#' positions and associated codelists.
#'
#' @param dataset An istat_dataset object
#' @param include_descriptions Logical; whether to include dimension descriptions (default: TRUE)
#'
#' @return A tibble with dimension information
#'
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#' ds <- istat_dataset("139_176")
#' dimensions_info(ds)
#' }
dimensions_info <- function(dataset, include_descriptions = TRUE) {
  dims_df <- purrr::map_df(dataset$dimensions, function(dim) {
    tibble::tibble(
      dimension_id = dim$id,
      position = dim$position,
      codelist_id = dim$codelist_id
    )
  })

  if (include_descriptions && nrow(dims_df) > 0) {
    # Get descriptions from codelists
    dims_df$description <- vapply(
      seq_len(nrow(dims_df)),
      function(i) {
        get_dimension_description(dims_df$codelist_id[i])
      },
      character(1)
    )
  }

  dims_df
}

#' Get description for a dimension from its codelist
#'
#' @param codelist_id Codelist ID
#'
#' @return Character string with the dimension description
#' @keywords internal
get_dimension_description <- function(codelist_id) {
  if (is.na(codelist_id)) {
    return(NA_character_)
  }

  tryCatch({
    path <- paste0("codelist/", get_agency_id(), "/", codelist_id)
    result <- istat_request_xml(path)
    xml <- result$xml
    ns <- result$ns

    codelist_node <- xml2::xml_find_first(xml, ".//structure:Codelist", ns)
    if (!is.na(codelist_node)) {
      get_name_by_lang_ns(codelist_node, "en", ns)
    } else {
      NA_character_
    }
  }, error = function(e) {
    NA_character_
  })
}

#' Get available values for a dimension
#'
#' Retrieves all available values for a specific dimension of a dataset.
#'
#' @param dataset An istat_dataset object
#' @param dimension_id The ID of the dimension
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Value ID/code}
#'     \item{name}{Human-readable name (English)}
#'   }
#'
#' @export
#'
#' @importFrom xml2 xml_find_all xml_attr
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#' ds <- istat_dataset("139_176")
#' get_dimension_values(ds, "TIPO_DATO")
#' }
get_dimension_values <- function(dataset, dimension_id) {
  if (!dimension_id %in% names(dataset$dimensions)) {
    stop("Dimension '", dimension_id, "' not found in dataset. ",
         "Available dimensions: ", paste(names(dataset$dimensions), collapse = ", "))
  }

  codelist_id <- dataset$dimensions[[dimension_id]]$codelist_id

  if (is.na(codelist_id)) {
    warning("No codelist found for dimension: ", dimension_id)
    return(tibble::tibble(id = character(), name = character()))
  }

  path <- paste0("codelist/", get_agency_id(), "/", codelist_id)
  result <- istat_request_xml(path)
  xml <- result$xml
  ns <- result$ns

  code_nodes <- xml2::xml_find_all(xml, ".//structure:Code", ns)

  purrr::map_df(code_nodes, function(code_node) {
    tibble::tibble(
      id = xml2::xml_attr(code_node, "id"),
      name = get_name_by_lang_ns(code_node, "en", ns)
    )
  })
}

#' Set filters for a dataset
#'
#' Sets dimension filters that will be used when retrieving data.
#' Filter names should match dimension IDs (case-insensitive).
#'
#' @param dataset An istat_dataset object
#' @param ... Named arguments where names are dimension IDs and values are
#'   either single values or character vectors for multiple values.
#'   Use "." to select all values for a dimension.
#'
#' @return The modified istat_dataset object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- istat_dataset("139_176")
#'
#' # Set single values
#' ds <- set_filters(ds, FREQ = "M", PAESE_PARTNER = "WORLD")
#'
#' # Set multiple values
#' ds <- set_filters(ds, TIPO_DATO = c("ISAV", "ESAV"))
#' }
set_filters <- function(dataset, ...) {
  filters <- list(...)

  # Get uppercase dimension names for matching
  dim_names_upper <- toupper(names(dataset$dimensions))
  dim_names_original <- names(dataset$dimensions)

  for (filter_name in names(filters)) {
    # Try case-insensitive matching
    idx <- match(toupper(filter_name), dim_names_upper)

    if (is.na(idx)) {
      warning("Dimension '", filter_name, "' not found in dataset. Ignoring.")
      next
    }

    actual_dim_name <- dim_names_original[idx]
    dataset$filters[[actual_dim_name]] <- filters[[filter_name]]
  }

  dataset
}

#' Reset all filters to default (all values)
#'
#' @param dataset An istat_dataset object
#'
#' @return The modified istat_dataset object with all filters reset to "."
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- istat_dataset("139_176")
#' ds <- set_filters(ds, FREQ = "M")
#' ds <- reset_filters(ds)  # All filters back to "."
#' }
reset_filters <- function(dataset) {
  for (dim_name in names(dataset$filters)) {
    dataset$filters[[dim_name]] <- "."
  }
  dataset
}

#' Get all available values for all dimensions
#'
#' Uses the availableconstraint endpoint to get all valid values for each dimension.
#' This is more accurate than getting values from codelists as it reflects
#' actual data availability.
#'
#' @param dataset An istat_dataset object
#'
#' @return A named list where each element contains a tibble of available values
#'   for that dimension
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- istat_dataset("139_176")
#' available <- get_available_values(ds)
#' available$FREQ  # Available frequency values
#' }
get_available_values <- function(dataset) {
  path <- paste0(
    "availableconstraint/",
    dataset$df_id,
    "?references=all&detail=full"
  )

  tryCatch({
    result <- istat_request_xml(path)
    xml <- result$xml
    ns <- result$ns

    # Find all KeyValue elements - try different namespace patterns
    key_values <- xml2::xml_find_all(xml, ".//structure:KeyValue", ns)
    if (length(key_values) == 0) {
      key_values <- xml2::xml_find_all(xml, ".//common:KeyValue", ns)
    }

    result_list <- list()
    for (kv in key_values) {
      dim_id <- xml2::xml_attr(kv, "id")

      # Try different namespace patterns for Value elements
      values <- xml2::xml_find_all(kv, ".//common:Value", ns)
      if (length(values) == 0) {
        values <- xml2::xml_find_all(kv, ".//structure:Value", ns)
      }

      value_ids <- vapply(values, function(v) {
        xml2::xml_text(v)
      }, character(1))

      result_list[[dim_id]] <- tibble::tibble(
        id = value_ids
      )
    }

    result_list
  }, error = function(e) {
    warning("Could not retrieve available values: ", e$message)
    list()
  })
}

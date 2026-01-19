#' @title ISTAT API Utility Functions
#' @description XML parsing and helper functions
#' @name utils
NULL

#' Extract text from XML node safely
#'
#' @param node XML node
#' @param xpath XPath expression
#' @param default Default value if not found
#'
#' @return Character string
#' @keywords internal
#'
#' @importFrom xml2 xml_find_first xml_text
xml_text_safe <- function(node, xpath, default = NA_character_) {
  result <- xml2::xml_find_first(node, xpath)
  if (is.na(result) || length(result) == 0) {
    return(default)
  }
  xml2::xml_text(result)
}

#' Extract attribute from XML node safely
#'
#' @param node XML node
#' @param attr Attribute name
#' @param default Default value if not found
#'
#' @return Character string
#' @keywords internal
#'
#' @importFrom xml2 xml_attr
xml_attr_safe <- function(node, attr, default = NA_character_) {
  result <- xml2::xml_attr(node, attr)
  if (is.na(result) || length(result) == 0) {
    return(default)
  }
  result
}

#' Build SDMX URL key from filters
#'
#' Creates an SDMX-compliant filter key string from a list of dimension filters.
#' Multiple values for a dimension are joined with "+", dimensions are separated by ".".
#'
#' @param filters Named list of filters where names are dimension IDs and values
#'   are either single values or character vectors for multiple values.
#'   Use "." to select all values for a dimension.
#'
#' @return Character string with the SDMX filter key
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Single values
#' make_url_key(list(FREQ = "M", TIPO_DATO = "ISAV"))
#' # Returns: "M.ISAV"
#'
#' # Multiple values for a dimension
#' make_url_key(list(FREQ = "M", TIPO_DATO = c("ISAV", "ESAV")))
#' # Returns: "M.ISAV+ESAV"
#' }
make_url_key <- function(filters) {
  if (length(filters) == 0) {
    return("")
  }

  key_parts <- vapply(filters, function(values) {
    if (length(values) == 1 && values == ".") {
      return(".")
    }
    if (length(values) > 1) {
      return(paste(values, collapse = "+"))
    }
    as.character(values)
  }, character(1))

  paste(key_parts, collapse = ".")
}

#' Parse language-specific name from XML
#'
#' @param node XML node containing Name elements
#' @param lang Language code (default: "en")
#'
#' @return Character string with the name in the specified language
#' @keywords internal
#'
#' @importFrom xml2 xml_find_all xml_attr xml_text
get_name_by_lang <- function(node, lang = "en") {
  names <- xml2::xml_find_all(node, ".//Name")
  for (name_node in names) {
    node_lang <- xml2::xml_attr(name_node, "lang")
    if (!is.na(node_lang) && node_lang == lang) {
      return(xml2::xml_text(name_node))
    }
  }
  # Fall back to first Name if language not found
  if (length(names) > 0) {
    return(xml2::xml_text(names[[1]]))
  }
  NA_character_
}

#' Parse language-specific name from XML with namespace support
#'
#' @param node XML node containing Name elements
#' @param lang Language code (default: "en")
#' @param ns XML namespace object
#'
#' @return Character string with the name in the specified language
#' @keywords internal
#'
#' @importFrom xml2 xml_find_all xml_attr xml_text
get_name_by_lang_ns <- function(node, lang = "en", ns = NULL) {
  # Try common:Name first (standard SDMX namespace)
  names <- xml2::xml_find_all(node, ".//common:Name", ns)

  for (name_node in names) {
    node_lang <- xml2::xml_attr(name_node, "lang")
    if (!is.na(node_lang) && node_lang == lang) {
      return(xml2::xml_text(name_node))
    }
  }
  # Fall back to first Name if language not found
  if (length(names) > 0) {
    return(xml2::xml_text(names[[1]]))
  }
  NA_character_
}

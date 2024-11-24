#validation.R

box::use(
  shinyvalidate[sv_required]
)

#' Validates that input is a positive integer
#' @param value Input value to validate
#' @return Error message if invalid, NULL if valid
#' @export
pos_int_validation <- function(value) {
  if (!is.numeric(value)) return("Input must be a number")
  if (value != round(value)) return("Input must be an integer")
  if (value <= 0) return("Input must be positive")
  return(NULL)
}

#' Validates text does not contain whitespace
#' @param value Input text to validate
#' @return Error message if invalid, NULL if valid
#' @export
no_whitespace_validation <- function(value) {
  if (any(grepl(" ", trimws(strsplit(value, ",")[[1]])))) {
    return("Input cannot contain whitespace")
  }
  return(NULL)
}

#' Validates comma-separated numbers
#' @param value Input text to validate
#' @return Error message if invalid, NULL if valid
#' @export
comma_sep_numbers_validation <- function(value) {
  if (any(grepl("[^0-9,]", value))) {
    return("Input must be a comma-separated list of numbers")
  }
  return(NULL)
}

#' Validates that number of comma-separated values matches expected count
#' @param value Input string with comma-separated values
#' @param expected_count Expected number of values
#' @param item_name Name of items being counted (for error message)
#' @return Error message if invalid, NULL if valid
#' @export
validate_value_count <- function(value, expected_count, item_name = "items") {
  values <- strsplit(value, ",")[[1]]
  count <- length(values)

  if (count != expected_count) {
    return(sprintf("Expected %d %s but found %d",
                  expected_count, item_name, count))
  }
  return(NULL)
}

#' Validates attribute names
#' @param value Comma-separated attribute names
#' @param expected_count Expected number of attributes
#' @return Error message if invalid, NULL if valid
#' @export
validate_attribute_names <- function(value, expected_count) {
  attribute_names <- strsplit(value, ",")[[1]]
  count <- length(attribute_names)

  if (any(trimws(attribute_names) == "")) {
    return("Attribute names cannot be just whitespace")
  }

  return(validate_value_count(value, expected_count, "attribute names"))
}

#' Validates number of items for each time point matches expected count
#' @param value Comma-separated item counts
#' @param expected_count Expected number of time points
#' @return Error message if invalid, NULL if valid
#' @export
validate_items_per_timepoint <- function(value, expected_count) {
  items <- as.numeric(unlist(strsplit(value, ",")))
  count <- length(items)

  if (count != expected_count) {
    return(sprintf("Expected %d numbers of items but found %d",
                   expected_count, count))
  }
  return(NULL)
}
box::use(
  shinyvalidate[sv_required],
  shiny[
    renderUI,
    div,
    tags,
    actionButton
  ]
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

#' Validates matrix dimensions against expected values
#' @param actual_rows Actual number of rows
#' @param actual_cols Actual number of columns
#' @param expected_rows Expected number of rows
#' @param expected_cols Expected number of cols
#' @return Error message if invalid, NULL if valid
#' @export
validate_matrix_dimensions <- function(actual_rows, actual_cols, expected_rows, expected_cols) {
  if (!is.null(expected_rows) && actual_rows != expected_rows) {
    return(sprintf(
      "The number of rows in the matrix (%d) does not match the expected number (%d) for the number
      of items at a single time point. Please ensure they are equal.",
      actual_rows, expected_rows
    ))
  }

  if (!is.null(expected_cols) && actual_cols != expected_cols) {
    return(sprintf(
      "The number of columns (%d) does not match the expected number (%d) for the number of 
      attributes. Please ensure they are equal.",
      actual_cols, expected_cols
    ))
  }

  return(NULL)
}

#' Renders error message UI and updates button state
#' @param output Shiny output object
#' @param session Shiny session
#' @param error_message Error message to display
#' @export
render_validation_ui <- function(output, session, error_message) {
  if (!is.null(error_message)) {
    output$errorBox <- renderUI({
      div(
        class = "alert alert-danger", role = "alert",
        tags$strong("Error: "), error_message
      )
    })
    output$nextButtonUI <- renderUI({
      actionButton(session$ns("nextButton"), "Next",
        class = "btn-primary disabled",
        disabled = TRUE
      )
    })
    return(FALSE)
  }

  output$errorBox <- renderUI(NULL)
  output$nextButtonUI <- renderUI({
    actionButton(session$ns("nextButton"), "Next", class = "btn-primary")
  })
  return(TRUE)
}

#' Validates IR matrix dimensions against Q matrix and time points
#' @param ir_cols Number of columns in IR matrix
#' @param q_rows Number of rows in Q matrix
#' @param time_points Number of time points
#' @return List with error message and expected time points if invalid, NULL if valid
#' @export
validate_ir_matrix_dimensions <- function(ir_cols, q_rows, time_points) {
  # Input validation
  if (is.null(ir_cols) || is.null(q_rows)) {
    return(NULL)
  }

  # Convert inputs to numeric
  ir_cols <- as.numeric(ir_cols)
  q_rows <- as.numeric(q_rows)
  time_points <- as.numeric(time_points)

  expected_time_points <- ir_cols / q_rows

  if (!is.null(time_points) && q_rows * time_points != ir_cols) {
    return(list(
      error = sprintf(
        "The number of columns in the IR matrix (%.0f) does not match the number of rows in the Q 
        matrix (%.0f) multiplied by the number of time points (%.0f). The number of time points 
        should be %.0f based on the IR matrix columns.",
        ir_cols, q_rows, time_points, expected_time_points
      ),
      expected_time_points = expected_time_points
    ))
  }

  return(NULL)
}
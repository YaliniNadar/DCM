#' @export
box::use(
  shiny[updateCheckboxInput],
  data.table[fread]
)

#' Read data file with given separator
#' @export
read_data_file <- function(file_path, separator = NULL, exclude_headers = FALSE) {
  base_params <- list(
    check.names = FALSE,
    quote = "",
    header = exclude_headers
  )

  sep_param <- if (is.null(separator) || separator == "__NULL__") {
    list()  # Default auto-detection
  } else {
    list(sep = separator)
  }

  params <- c(
    list(file_path),
    base_params,
    sep_param
  )

  do.call(fread, params)
}

#' Detect if data has row IDs in first column
#' @export
has_row_ids <- function(data_sample) {
  if (ncol(data_sample) < 2) return(FALSE)
  
  first_col <- data_sample[[1]]
  rest_cols <- data_sample[, -1]
  
  first_col_non_numeric <- suppressWarnings(any(is.na(as.numeric(first_col))))
  first_col_unique <- length(unique(first_col)) == length(first_col)
  
  rest_numeric <- all(sapply(rest_cols, function(x) {
    all(!is.na(suppressWarnings(as.numeric(as.character(x)))))
  }))
  
  return((first_col_non_numeric || first_col_unique) && rest_numeric)
}

#' Detect if data has headers in first row
#' @export
has_header <- function(data_sample) {
  if (nrow(data_sample) < 2) return(FALSE)
  
  first_row <- data_sample[1,]
  rest_data <- data_sample[-1,]
  
  first_row_types <- sapply(first_row, function(x) {
    num_conversion <- suppressWarnings(as.numeric(x))
    is.na(num_conversion)
  })
  
  rest_numeric <- all(sapply(rest_data, function(x) {
    all(suppressWarnings(!is.na(as.numeric(x))))
  }))
  
  return(any(first_row_types) && rest_numeric)
}

#' Process matrix file and update UI
#' @export
process_matrix_file <- function(file, input, session, separator) {
  if (is.null(file$datapath)) return(NULL)
  
  # First read to check format
  sample_data <- read_data_file(
    file_path = file$datapath,
    separator = separator,
    exclude_headers = FALSE
  )
  
  # Auto-detect headers
  if (!isTRUE(input$excludeHeaders) && has_header(sample_data)) {
    updateCheckboxInput(session, "excludeHeaders", value = TRUE)
  }
  
  # Auto-detect row IDs
  if (!isTRUE(input$excludeIdColumns) && has_row_ids(sample_data)) {
    updateCheckboxInput(session, "excludeIdColumns", value = TRUE)
  }
  
  # Read final data
  data_temp <- read_data_file(
    file_path = file$datapath,
    separator = separator,
    exclude_headers = input$excludeHeaders
  )
  
  if (input$excludeIdColumns) {
    data_temp <- data_temp[, -1, with = FALSE]
  }
  
  return(data_temp)
}
box::use(
  TDCM[
    tdcm,
    data.tdcm01,
    tdcm.summary,
    tdcm.plot
  ],
  utils[
    data,
    str,
  ],
  memoise[
    memoise
  ],
  stringr[
    str_squish
  ],
  data.table[
    as.data.table,
    data.table
  ],
  graphics[
    plot,
    points,
    arrows,
    barplot
  ]
)

fit_and_summarize <- memoise(function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model <- if (rule == "full LCDM" || rule == "Different on each item") { # Remove this when Different on each item is implemented
      tdcm(ir_matrix, q_matrix, num.time.points = time_pts, invariance = invariance)
    } else {
      tdcm(ir_matrix, q_matrix, num.time.points = time_pts, invariance = invariance, rule = rule)
    }
    # Check if attribute_names is not NULL
    if (!is.null(attribute_names)) {
      results <- tdcm.summary(model, num.time.points = time_pts, attribute.names = attribute_names)
    } else {
      results <- tdcm.summary(model, num.time.points = time_pts)
    }
    return(results)
  }
})

#' @export
convert_to_datatable <- function(data) {
  return(as.data.table(data, keep.rownames = TRUE))
}

#' @export
get_misc_datatable <- function(result) {
  # Create a data.table containing specific elements
  misc_data <- data.table(
    Name = c("Mean.Item.RMSEA", "loglike", "deviance", "AIC", "BIC", "CAIC", "Npars"),
    Value = sapply(c(
      "Mean.Item.RMSEA",
      "loglike", "deviance",
      "AIC", "BIC", "CAIC",
      "Npars"
    ), function(name) result[[name]])
  )
  return(misc_data)
}


convert_to_char_data_table <- function(new_data_frame) {
  new_data_frame_nrow <- nrow(new_data_frame)
  new_data_frame_ncol <- ncol(new_data_frame)

  new_data_frame_rownames <- rownames(new_data_frame)
  new_data_frame_colnames <- colnames(new_data_frame)

  new_data_frame_matrix <- matrix(
    data = as.matrix(new_data_frame),
    nrow = new_data_frame_nrow,
    ncol = new_data_frame_ncol,
    dimnames = list(new_data_frame_rownames, new_data_frame_colnames)
  )

  new_data_frame_data_table <- as.data.table(
    new_data_frame_matrix,
    keep.rownames = TRUE
  )

  return(new_data_frame_data_table)
}

item_parameters_as_data_table <- function(item_parameters) {
  item_parameters_nrow <- nrow(item_parameters)
  item_parameters_ncol <- ncol(item_parameters)

  item_parameters_rownames <- rownames(item_parameters)
  item_parameters_colnames <- colnames(item_parameters)

  item_parameters_double <- sapply(as.matrix(item_parameters), function(value) {
    value <- str_squish(value)
    if (identical(value, "--")) {
      value <- NA_real_
    } else {
      value <- as.double(value)
    } # if
    return(value)
  })

  names(item_parameters_double) <- NULL

  item_parameters_matrix <- matrix(
    data = item_parameters_double,
    nrow = item_parameters_nrow,
    ncol = item_parameters_ncol,
    dimnames = list(item_parameters_rownames, item_parameters_colnames)
  )

  item_parameters_data_table <- as.data.table(
    item_parameters_matrix,
    keep.rownames = TRUE
  )

  return(item_parameters_data_table)
}

#' @export
get_attribute_names <- function(q_matrix) {
  # Check if the data frame has column names
  has_headers <- !all(names(q_matrix) == "")

  if (has_headers) {
    print("The DataTable has headers.")
    headers <- names(q_matrix)
    return(headers)
  } else {
    print("The DataTable does not have headers.")
  }
}

#' @export
item_parameters <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
    item_parameters <- item_parameters_as_data_table(results$item.parameters)
    return(item_parameters)
  }
}

#' @export
growth <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
    growth <- results$growth
    return(growth)
  }
}

#' @export
visualize <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule, type) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
    plot <- tdcm.plot(results, type = type)
    return(plot)
  }
}

#' @export
trans_prob <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  probs <- results$transition.probabilities
  return(probs)
}

#' @export
att_class <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  classifications <- results$classifications
  return(classifications)
}

#' @export
most_likely_trans <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  most_likely_trans <- convert_to_char_data_table(results$most.likely.transitions)
  return(most_likely_trans)
}

#' @export
trans_pos <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  trans_pos <- results$posterior.probabilities
  return(trans_pos)
}

#' @export
model_fit <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  model_fit <- results$model.fit
  return(model_fit)
}

#' @export
att_corr <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  att_corr <- results$att.corr
  return(att_corr)
}

#' @export
reliability <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)
  rel <- convert_to_datatable(results$reliability)
  return(rel)
}

#' Helper function to extract data from array
extract_growth_data <- function(growth_array) {
  # Check dimensions
  if (!is.array(growth_array) || length(dim(growth_array)) != 2) {
    warning("Expected 2D array for growth data")
    return(NULL)
  }

  # Extract dimensions
  n_timepoints <- dim(growth_array)[1]
  n_measures <- dim(growth_array)[2]

  # Create data frame
  data.frame(
    Time = seq_len(n_timepoints),
    Mean = growth_array[, 2],
    SE = growth_array[, 3]
  )
}

#' @export
visualize_line <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  message("Visualize line called with: ",
          "time_pts=", time_pts,
          " attr_names=", paste(attribute_names, collapse = ","))

  if (!is.null(q_matrix) && !is.null(ir_matrix) &&
        !is.null(time_pts) && !is.null(attribute_names)) {

    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)

    if (!is.null(results) && !is.null(results$growth)) {
      # Debug results structure
      message("Results growth structure: ", class(results$growth))
      message("Growth dim: ", paste(dim(results$growth), collapse = ","))

      # Handle different data types
      growth_df <- try({
        if (is.data.frame(results$growth)) {
          results$growth
        } else if (inherits(results$growth, "array") || is.matrix(results$growth)) {
          extract_growth_data(results$growth)
        } else {
          warning("Unsupported growth data type")
          return(NULL)
        }
      })

      if (inherits(growth_df, "try-error") || is.null(growth_df)) {
        warning("Error converting growth data")
        return(NULL)
      }

      # Create plot
      graphics::plot(growth_df$Time,
                     growth_df$Mean,
                     type = "b",
                     ylim = c(0, 1),
                     xlab = "Time Point",
                     ylab = "Probability",
                     main = "Growth Trajectories")

      if (!all(is.na(growth_df$SE))) {
        graphics::arrows(growth_df$Time,
                         growth_df$Mean - growth_df$SE,
                         growth_df$Time,
                         growth_df$Mean + growth_df$SE,
                         code = 3,
                         angle = 90,
                         length = 0.05)
      }

      return(invisible())
    }
  }
  warning("Invalid input parameters")
  return(NULL)
}

#' @export
visualize_bar <- function(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule) {
  message("Visualize bar called with: ",
          "time_pts=", time_pts,
          " attr_names=", paste(attribute_names, collapse = ","))

  if (!is.null(q_matrix) && !is.null(ir_matrix) &&
        !is.null(time_pts) && !is.null(attribute_names)) {

    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts, attribute_names, invariance, rule)

    if (!is.null(results) && !is.null(results$growth)) {
      # Debug results structure
      message("Results growth structure: ", class(results$growth))
      message("Growth dim: ", paste(dim(results$growth), collapse = ","))

      # Handle different data types
      growth_df <- try({
        if (is.data.frame(results$growth)) {
          results$growth
        } else if (inherits(results$growth, "array") || is.matrix(results$growth)) {
          extract_growth_data(results$growth)
        } else {
          warning("Unsupported growth data type")
          return(NULL)
        }
      })

      if (inherits(growth_df, "try-error") || is.null(growth_df)) {
        warning("Error converting growth data")
        return(NULL)
      }

      # Create plot
      bp <- graphics::barplot(growth_df$Mean,
                              ylim = c(0, max(growth_df$Mean + growth_df$SE, na.rm = TRUE)),
                              names.arg = growth_df$Time,
                              xlab = "Time Point",
                              ylab = "Probability",
                              main = "Growth by Time Point")

      if (!all(is.na(growth_df$SE))) {
        graphics::arrows(bp,
                         growth_df$Mean - growth_df$SE,
                         bp,
                         growth_df$Mean + growth_df$SE,
                         code = 3,
                         angle = 90,
                         length = 0.05)
      }

      return(invisible())
    }
  }
  warning("Invalid input parameters")
  return(NULL)
}
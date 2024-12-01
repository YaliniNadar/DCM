box::use(
  # Core Shiny imports
  shiny[
    NS, fluidPage, h2, fileInput, radioButtons, textInput,
    checkboxInput, moduleServer, observe, observeEvent,
    renderUI, uiOutput, div, actionButton, tagList,
    HTML, tags, updateCheckboxInput
  ],
  # Additional packages
  DT[DTOutput, renderDT, datatable, JS],
  # Custom modules
  app/logic[storage, validation, matrix_utils],
  app/view[ui_components, table_helper]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload IR-Matrix File"),
    ui_components$page_progress_bar(id, total_steps = 5, current_step = 1),

    fileInput(ns("fileIR"), "Choose IR-Matrix File"),
    radioButtons(ns("separatorType"), "Separator Type:",
      choices = c(
        "Try to Guess" = "__NULL__",
        "Tab" = "\t",
        "Comma" = ",",
        "Space" = " ",
        "Custom" = ""
      ),
      selected = "__NULL__"
    ),
    uiOutput(ns("custom_separator_input")),
    checkboxInput(ns("excludeHeaders"), "First Row Contains Column Names", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "First Column Contains Row IDs", value = FALSE),
    uiOutput(ns("errorBox")),
    DTOutput(ns("filePreviewIR")),
    div(
      style = "display: flex; justify-content: flex-end;",
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI"))
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$custom_separator_input <- renderUI({
      if (input$separatorType == "") {
        tagList(
          textInput(session$ns("customSeparator"), "Enter Custom Separator:"),
          tags$script(HTML(sprintf("$(document).on('shiny:inputchanged', function(event) {
            if (event.name === '%s') {
              $('#%s').attr('maxlength', 1);
            }
          });", session$ns("customSeparator"), session$ns("customSeparator"))))
        )
      }
    })

    output$nextButtonUI <- renderUI({
      ns <- session$ns
      if (!is.null(input$fileIR) && input$fileIR$size > 0) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary")
      } else {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })

    observeEvent(input$nextButton, {
      shiny.router::change_page("model_specs")
    })

    observe({
      separator <- if (input$separatorType == "") input$customSeparator else input$separatorType
      
      data_temp <- matrix_utils$process_matrix_file(input$fileIR, input, session, separator)
      
      if (!is.null(data_temp)) {
        num_cols_in_ir_matrix <- ncol(data_temp)
        num_rows_in_q_matrix <- data$num_rows_in_q_matrix
        num_time_points <- data$numTimePoints

        validation_result <- validation$validate_ir_matrix_dimensions(
          num_cols_in_ir_matrix,
          num_rows_in_q_matrix,
          num_time_points
        )

        is_valid <- validation$render_validation_ui(output, session, validation_result$error)
        
        if (is_valid) {
          data$ir_matrix <- data_temp
          output$filePreviewIR <- renderDT({
            datatable(data_temp,
              options = list(
                autoWidth = TRUE,
                scrollX = TRUE,
                searching = FALSE,
                initComplete = JS(table_helper$format_pagination())
              )
            )
          })
        } else {
          data$ir_matrix <- NULL
          output$filePreviewIR <- renderDT(NULL)
        }
      } else {
        output$filePreviewIR <- renderDT(NULL)
      }
    })

    ui_components$nb_server("nextButton", "model_specs")
  })
}
box::use(
  # Core Shiny imports
  shiny[
    actionButton,
    br,
    checkboxInput,
    div,
    fileInput,
    fluidPage,
    h2,
    HTML,
    moduleServer,
    NS,
    observe,
    observeEvent,
    radioButtons,
    renderUI,
    tagList,
    tags,
    textInput,
    uiOutput
  ],

  # Additional packages
  DT[
    datatable,
    DTOutput,
    JS,
    renderDT
  ],
  data.table[fread],
  shinyjs[useShinyjs],

  # Custom modules
  app/logic[
    storage,
    validation
  ],
  app/view[
    table_helper,
    ui_components
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload Q-Matrix File"),
    br(),
    tags$div(
      style = "display: flex; flex-direction: row; justify-content: space-between;",
      tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
      tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: #ffc400;"),
      tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
      tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
      tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
    ),
    br(),


    # Input: Upload Q-Matrix file
    fileInput(ns("fileQ"), "Choose Q-Matrix File"),

    # Input: Separator type
    radioButtons(ns("separatorType"), "Separator Type:",
      choices = c("Tab" = "\t", "Comma" = ",", "Space" = " ", "Custom" = ""),
      selected = ","
    ),

    # Conditional Separator
    uiOutput(ns("custom_separator_input")),

    # Input: Additional options
    checkboxInput(ns("excludeHeaders"), "First Row Contains Column Names", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "First Column Contains Row IDs", value = FALSE),
    uiOutput(ns("errorBox")),

    # File preview using DTOutput
    DTOutput(ns("filePreviewQ")),
    div(
      style = "display: flex; justify-content: flex-end;", # Aligns the buttons to the right
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI")), # Placeholder for dynamic Next button rendering
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    shinyjs::useShinyjs()

    observeEvent(input$num_rows_in_q_matrix, {
      num_rows_in_q_matrix <- input$num_rows_in_q_matrix
    })
    # Conditional Rendering for Custom Separator
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

    # Dynamic rendering for the Next button based on file input
    output$nextButtonUI <- renderUI({
      ns <- session$ns # Ensure we have the namespace function available

      if (is.null(input$fileQ)) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })

    # Observe the Next button click event
    observeEvent(input$nextButton, {
      # Navigate to the q_matrix page
      shiny.router::change_page("ir_matrix")
    })

    observe({
      # Read the uploaded file
      file <- input$fileQ
      if (!is.null(file$datapath)) {
        separator <- input$separatorType
        if (separator == "") {
          data_temp <- fread(file$datapath,
            sep = input$customSeparator,
            header = input$excludeHeaders,
            check.names = FALSE,
            quote = "",
          )
        } else {
          data_temp <- fread(file$datapath,
            sep = input$separatorType,
            header = input$excludeHeaders,
            quote = "",
          )
        }

        # Exclude ID columns if specified
        if (input$excludeIdColumns) {
          # Define which columns to exclude (e.g., first column)
          id_columns <- 1
          # Remove ID columns from the DataTable
          data_temp <- data_temp[, -id_columns, with = FALSE]
        }

        # Validate the dimensions of the Q matrix
        observeEvent(input$fileQ, {
          num_cols_in_q_matrix <- ncol(data_temp)
          num_rows_in_q_matrix <- nrow(data_temp)
          num_attributes <- data$numAttributes
          num_items_for_single_time <- data$numTimeSinglePoint
          data$num_rows_in_q_matrix <- nrow(data_temp)

          error_message <- validation$validate_matrix_dimensions(
            num_rows_in_q_matrix,
            num_cols_in_q_matrix,
            num_items_for_single_time,
            num_attributes
          )

          is_valid <- validation$render_validation_ui(output, session, error_message)
          if (is_valid) {
            data$q_matrix <<- data_temp
          }
        })


        # Display file preview using DT
        output$filePreviewQ <- renderDT({
          datatable(data_temp,
            options = list(
              searching = FALSE,
              initComplete = JS(table_helper$format_pagination())
            )
          )
        })

        # Save the modified data to q_matrix
        data$q_matrix <<- data_temp
      } else {
        # Clear the preview if no file is selected
        output$filePreviewQ <- renderDT(NULL)
      }
    })
    observe({
      db_name <- Sys.getenv("DB_NAME")
      prefix <- "app-q_matrix-"
      fields <- c("separatorType", "excludeHeaders", "excludeIdColumns", "fileQ")
      storage$performIndexedDBRead(db_name, prefix, fields)
    })

    ui_components$nb_server("nextButton", "ir_matrix")
  })
}

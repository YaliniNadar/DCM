box::use(
  shiny[
    moduleServer,
    NS,
    fluidPage,
    titlePanel,
    mainPanel,
    div,
    p,
    fluidRow,
    column,
    br,
    actionButton,
    observeEvent,
    h1,
    reactiveValues,
    tags
  ],
  shinyBS[
    bsModal,
    toggleModal
  ],
  shiny.router[change_page],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h1("TDCMApp", align = "center"),
    br(),
    p(
      "TDCMApp provides a user-friendly interface to help 
      you gain valuable insights from your test data.",
      align = "center"
    ),
    br(),
    mainPanel(
      fluidRow(
        # Single Group Column
        column(
          6,
          p(
            "Choose Single Group if you have only one group of students 
            who took the test. Ideal for assessing the overall performance 
            of a general student population."
          ),
          br(),
          actionButton(
            inputId = ns("go_to_param_specs"),
            label = "Single Group",
            class = "btn-primary btn-md"
          )
        ),
        # Multi Group Column
        column(
          6,
          p(
            "Choose Multi Group when evaluating multiple groups of students who took the test. 
            Especially useful for scenarios like educational interventions, where you may have 
            distinct control and treatment groups."
          ),
          br(),
          actionButton(
            inputId = ns("button2"),
            label = "Multi Group",
            class = "btn-primary btn-md disabled",
            style = "opacity: 0.65; cursor: not-allowed;",
            disabled = TRUE
          ),
          br(),
          p(
            "Multi Group functionality will be available in a future update.",
            style = "color: grey; margin-top: 10px;"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          12,
          actionButton(
            inputId = ns("help_button"),
            label = "Need Help Choosing?",
            class = "btn-secondary",
          )
        )
      )
    ),

    # Modals
    bsModal(
      id = ns("coming_soon_modal"),
      title = "Coming Soon",
      trigger = ns("button2"),
      size = "small",
      "The Multi Group feature is under development and will be available in a future update."
    ),

    bsModal(
      id = ns("help_modal"),
      title = "Help: Choosing Between Single and Multi Group",
      trigger = ns("help_button"),
      size = "large",
      p(
        "Use the following guidelines to select the appropriate option:",
        tags$ul(
          tags$li("Select Single Group if you are analyzing data from a single, homogeneous group 
          of students without any subdivisions."),
          tags$li("Select Multi Group (when available) if you need to compare different subgroups, 
          such as control and treatment groups, to assess the impact of specific interventions.")
        )
      )
    )
  )
}

#' @export
server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_to_param_specs, {
      change_page("param_specs")
    })

    # Handle clicks on the disabled "Multi Group" button
    observeEvent(input$button2, {
      toggleModal(session, "coming_soon_modal", toggle = "open")
    })

    # Show a helper modal when the "Need Help Choosing?" button is clicked
    observeEvent(input$help_button, {
      toggleModal(session, "help_modal", toggle = "open")
    })
  })
}

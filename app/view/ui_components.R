box::use(
  shiny[
    navbarPage,
    tabPanel,
    NS,
    actionButton,
    observeEvent,
    moduleServer,
    HTML,
    showModal,
    modalDialog,
    modalButton,
    tagList,
    div,
    tags,
    icon
  ],
  shiny.router[change_page, route_link],
  rintrojs[introjsUI, introjs, introBox],
)

#' @export
navbar_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = "TDCMApp",
    id = ns("navbar")
  )
}

#' @export
next_button <- function(id, enabled = TRUE) {
  ns <- NS(id)

  if (enabled) {
    actionButton(
      ns("nextButton"),
      "Next",
      class = "btn-primary",
      style = "float: right; margin-right: 10px;"
    )
  } else {
    tags$button(
      "Next",
      id = ns("nextButton"),
      class = "btn btn-primary disabled",
      style = "float: right; margin-right: 10px;",
      disabled = "disabled"
    )
  }
}

#' @export
back_button <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("backButton"),
    "Back",
    class = "btn-primary",
    style = "float: right; margin-right: 5px; margin-left: 5px;",
    onclick = "App.goBack();"
  )
}

#' @export
reset_button <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("resetBtn"),
    "Back",
    class = "btn-primary",
    style = "float: right; margin-right: 5px; margin-left: 5px;",
  )
}

#' @export
rb_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$resetBtn, {
      print("reset button is clicked")
      # Reset all variables
      # Add more reset actions for other variables as needed
      showModal(modalDialog(
        title = "Confirm Navigation",
        HTML("Are you sure you want to leave this page?<br/><br/>
         This action will erase all entered data requiring you to start over.<br/>
         Make sure to download any file(s) you need before leaving."),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("confirmLeave"), "Yes, Leave"),
          modalButton("No, Stay"),
        )
      ))
    })
    observeEvent(input$confirmLeave, {
      change_page("/")
      session$reload()
    })
  })
}

#' @export
nb_server <- function(id, route) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$nextButton, {
      change_page(route)
    })
  })
}

#' Progress bar UI Component
#'
#' @param id The id of the progress bar
#' @param total_steps The total number of steps in the progress bar
#' @return A shiny UI component
#' @export
progress_bar_ui <- function(id, total_steps = 5) {
  ns <- NS(id)

  div(
    class = "progress",
    style = "height: 20px; margin-bottom: 20px;",
    div(
      class = "progress-bar",
      role = "progressbar",
      "aria-valuenow" = "0",
      "aria-valuemin" = "0",
      "aria-valuemax" = total_steps,
      style = "width: 0%;",
      id = ns("progressBar")
    )
  )
}

#' Help Intro Box
#'
#' @param id Namespace ID
#' @return Shiny UI elements for the help intro box
#' @export
help_intro_box <- function(id) {
  ns <- NS(id)

  introBox(
    actionButton(ns("startTour"), "Help", class = "btn-primary",
      data.step = 1,
      data.intro = "Click this button to start the tour.",
    )
  )
}

#' Tour UI Component
#'
#' @param id Namespace ID
#' @return Shiny UI elements for the tour
#' @export
tour_ui <- function(id) {
  ns <- NS(id)

  tagList(
    help_intro_box(id)
  )
}

#' Factory function to create tour steps
#'
#' @param id Namespace ID
#' @param ui_element The UI element to be displayed in the tour step
#' @param step_number The step number of the tour
#' @param intro_text The text to be displayed in the tour step
#' @return A shiny UI element wrapped in introBox
#' @export
create_tour_step <- function(id, ui_element, step_number, intro_text) {
  ns <- NS(id)
  introBox(
    ui_element,
    data.step = step_number,
    data.intro = intro_text
  )
}

#' Create a page progress bar
#' @param id Module ID
#' @param total_steps Total number of steps
#' @param current_step Current step number
#' @export
page_progress_bar <- function(id, total_steps, current_step) {
  ns <- NS(id)

  # Input validation
  current_step <- min(max(1, current_step), total_steps)

  # Create progess bar
  tags$div(class = "progress-bar-container", 
    lapply(1:total_steps, function(step) {
      tags$div(class = ifelse(step == current_step, "page current-page", "page")
      )
    })
  )
}

#' Results Navigation Component
#' @param id Module ID
#' @param current_tab Current active tab name
#' @return Navigation UI element
#' @export
results_navigation <- function(id, current_tab) {
  ns <- NS(id)

  nav_items <- list(
    list(
      title = "Aggregate Results",
      route = "primary_aggregate_results",
      icon = "chart-bar"
    ),
    list(
      title = "Individual Results",
      route = "primary_individual_results",
      icon = "users"
    ),
    list(
      title = "Secondary Results",
      route = "secondary_results",
      icon = "chart-pie"
    )
  )

  div(
    class = "results-nav",
    style = "margin-bottom: 2rem; border-bottom: 1px solid #dee2e6;",

    div(
      class = "nav nav-tabs",
      style = "display: flex; gap: 1rem; padding: 0.5rem 1rem;",

      lapply(nav_items, function(item) {
        is_active <- item$route == current_tab

        tags$a(
          href = route_link(item$route),
          class = sprintf(
            "nav-item nav-link %s",
            if (is_active) "active" else ""
          ),
          style = paste(
            "display: flex;",
            "align-items: center;",
            "gap: 0.5rem;",
            "padding: 0.75rem 1.25rem;",
            "color:", if (is_active) "#007bff" else "#6c757d", ";",
            "border-bottom:", if (is_active) "2px solid #007bff" else "none", ";",
            "text-decoration: none;"
          ),

          icon(item$icon),
          item$title
        )
      })
    )
  )
}
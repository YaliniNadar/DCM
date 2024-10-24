if (file.exists("renv")) {
  source("renv/activate.R")
  message("renv activated")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

build_and_run_app <- function() {
  rhino::build_sass(watch = TRUE)
  shiny::runApp()
}

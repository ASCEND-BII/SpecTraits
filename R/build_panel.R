################################################################################
### Build panel

build_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Welcome to the Build Panel"),
    actionButton(ns("go_to_home"), "Go back to Home")  # Button to switch back to Home
  )
}

# Analysis panel module Server logic
build_panel_server <- function(id, updateTab) {
  moduleServer(id, function(input, output, session) {

    # Navigate back to Home panel when the button is clicked
    observeEvent(input$go_to_home, {
      updateTab("Build")
    })
  })
}

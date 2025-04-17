################################################################################
##### Trait selector
################################################################################

trait_selector_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("column_ui"))
}

trait_selector_sever <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$column_ui <- renderUI({
      req(data())
      numeric_cols <- names(data())[sapply(data(), is.numeric)]
      selectInput(session$ns("column"), "Select trait", choices = numeric_cols)
    })

    reactive({
      req(input$column)
      input$column
    })
  })
}

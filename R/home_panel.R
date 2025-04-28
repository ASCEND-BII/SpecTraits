################################################################################
### Home panel

# Home panel module UI
home_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(column(width = 10,
                  offset = 1,
                  wellPanel(align = "justify",
                            HTML("<h1 align = 'center'>SpecTraits 0.1 </h1>"),
                            p(""),
                            p(""),
                            p(""),
                            HTML("<h4 align = 'center'>SpecTraits is a Shiny application designed to predict and build models for inferring leaf traits using leaf spectroscopy as well as pre-process spectra and access to data"),
                            p(""),
                            HTML("<h4 align = 'center'>SpecTraits offers four main functionalities:"),
                            p(""),
                            p(""),
                            p(""),
                            div(
                              style = "text-align: center;",
                            fluidRow(width = 12,
                                     div(
                                       class = "center-buttons",
                                       actionButton(ns("go_to_predict"), "Go to Predict"),
                                       actionButton(ns("go_to_build"), "Go to Build"),
                                       actionButton(ns("go_to_prepro"), "Go to Pre-processing"))
                            )),
                            p(""),
                            p(""),
                            p(""),
                            div(
                              style = "text-align: center;",
                            img(src = "plsr.png", width = "400px", height = "320px", align = "center")
                            ),
                            p(""),
                            p(""))

                  )
  )
}

# Home panel module Server logic
home_panel_server <- function(id, go_to_predict_react, go_to_build_react, go_to_prepro_react) {
  moduleServer(id, function(input, output, session) {

    # Navigate to Predictions panel when the corresponding button is clicked
    observeEvent(input$go_to_predict, {
      go_to_predict_react(TRUE)
    })

    # Navigate to Analysis panel when the corresponding button is clicked
    observeEvent(input$go_to_build, {
      go_to_build_react(TRUE)
    })

    # Navigate to Predictions panel when the corresponding button is clicked
    observeEvent(input$go_to_prepro, {
      go_to_prepro_react(TRUE)
    })

  })
}

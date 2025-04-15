################################################################################
### Home panel

# Home panel module UI
home_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(column(width = 6,
                  offset = 3,
                  wellPanel(align = "justify",
                            HTML("<h1 align = 'center'>SpecTraits 0.1 </h1>"),
                            h4(),
                            HTML("<p>SpecTraits is a Shiny application designed to predict and build models for inferring leaf traits using reflectance spectra"),
                            p("SpecTraits offers three main functionalities:
                                                                  i) predicting leaf traits using user-provided PLSR coefficients and Radiative Transfer Models (RTMs),
                                                                  ii) building PLSR models from a user-defined dataset of leaf traits and reflectance spectra,
                                                                  iii) basic pre-processing of reflectance spectra.
                                                                "),
                            p(""),
                            p(""),
                            p(""),
                            div(
                              style = "text-align: center;",
                            fluidRow(
                              column(12, actionButton(ns("go_to_predict"), "Go to Predict"),
                                         actionButton(ns("go_to_build"), "Go to Build"),
                                         actionButton(ns("go_to_prepro"), "Go to Pre-processing"))
                            ),
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

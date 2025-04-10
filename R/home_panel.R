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
                            HTML("<p>SpecTraits is a shiny application aiming at predicting and building models to infer about leaf traits using reflectance spectra"),
                            p("SpecTraits has two main functionalities:
                                                                  i) predicting leaf traits from users or publised PLSR coefficients and Radiative Transfer Models (RTM), and
                                                                  ii) build PLSR models from a user-defined data set of leaf traits-spectra.
                                                                "),
                            p(""),
                            p(""),
                            p(""),
                            div(
                              style = "text-align: center;",
                            fluidRow(
                              column(6, actionButton(ns("go_to_predict"), "Go to Predict")),  # Button to switch to Predictions
                              column(6, actionButton(ns("go_to_build"), "Go to Build"))  # Button to switch to Analysis /, class = "btn-primary"
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
home_panel_server <- function(id, go_to_predict_react, go_to_build_react) {
  moduleServer(id, function(input, output, session) {

    # Navigate to Predictions panel when the corresponding button is clicked
    observeEvent(input$go_to_predict, {
      go_to_predict_react(TRUE)
    })

    # Navigate to Analysis panel when the corresponding button is clicked
    observeEvent(input$go_to_build, {
      go_to_build_react(TRUE)
    })
  })
}

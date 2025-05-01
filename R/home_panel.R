################################################################################
### Home panel

# Home panel module UI
home_panel_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    br(" "),
    HTML("<h1 align='center' style='color:#005F5F; font-weight:bold;'>SpecTraits 0.1</h1>"),
    br(" "),
    br(" "),
    HTML("<h4 align='center' style='color:#005F5F;'>SpecTraits is a Shiny application designed to predict
         and build models for inferring leaf traits using leaf spectroscopy as well
         as pre-process spectra and access to data"),
    br(" "),
    HTML("<h4 align='center' style='color:#005F5F;'>SpecTraits offers four main functionalities:"),
    br(" "),
    layout_columns(
      card(
        card_header(
          HTML("<h4 align='center' style='color:#005F5F; font-weight:bold;'>Predict")
        ),
        card_body(
          h5("Predict leaf traits using user-provided Partial Least Squares Regression
            coefficients or Radiative Transfer Models"),
          tags$div(
            style = "text-align: center; margin-top: 1px;",
            tags$img(src = "predict.png",
                     width = "50%")
          )
        )
      ),

      card(
        card_header(
          HTML("<h4 align='center' style='color:#005F5F; font-weight:bold;'>Build")
        ),
        card_body(
          h5("Build Partial Least Squares Regression (PLSR) models from a
            user-defined dataset of leaf traits and spectra"),
          tags$div(
            style = "text-align: center; margin-top: 1px;",
            tags$img(src = "build.png",
                     width = "50%"),
          )
        )
      ),

      card(
        card_header(
          HTML("<h4 align='center' style='color:#005F5F; font-weight:bold;'>Pre-process")
        ),
        card_body(
          h5("Apply basic pre-processing methods on leaf spectra (e.g., resampling, transformations)."),
          tags$div(
            style = "text-align: center; margin-top: 1px;",
            tags$img(src = "preprocessing.png",
                     width = "50%")
          )
        )
      ),

      card(
        card_header(
          HTML("<h4 align='center' style='color:#005F5F; font-weight:bold;'>Data")
        ),
        card_body(
          h5("Download curated datasets that integrate leaf traits and spectra"),
          tags$div(
            style = "text-align: center; margin-top: 1px;",
            tags$img(src = "data.png",
                     width = "50%"),
          )
        )
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

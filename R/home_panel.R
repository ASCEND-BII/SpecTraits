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
                                                                  i) predicting leaf traits from users or publised PLSR coefficients and radiative transfer models, and
                                                                  ii) build PLSR models from user-defined data set of leaf traits-spectra.
                                                                "),
                            p(""),
                            p(""),
                            p(""),
                            div(
                              style = "text-align: center;",
                            fluidRow(
                              column(6, actionButton(ns("go_to_predictions"), "Go to Predict Panel", class = "btn-primary")),  # Button to switch to Predictions
                              column(6, actionButton(ns("go_to_build"), "Go to Build Panel", class = "btn-primary"))  # Button to switch to Analysis
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
home_panel_server <- function(id, updateTab) {
  moduleServer(id, function(input, output, session) {

    # Navigate to Predictions panel when the corresponding button is clicked
    observeEvent(input$go_to_predictions, {
      #updateTab("Predict")
      updateTabsetPanel(session$parent, "tabs", selected = "Predict")
    })

    # Navigate to Analysis panel when the corresponding button is clicked
    observeEvent(input$go_to_build, {
      #updateTab("Build")
      updateTabsetPanel(session$parent, "tabs", selected = "Build")
    })
  })
}


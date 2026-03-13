################################################################################
### Spectral smoothing settings input module (Savitzky-Golay filter)
################################################################################

smoothing_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Define parameters for Savitzky-Golay smoothing filter"),
    p("The filter smooths spectra using a polynomial fit within a moving window."),
    numericInput(ns("window"), "Window size (must be odd)", value = 11, min = 3, step = 2),
    numericInput(ns("deriv_order"), "Derivative order", value = 0, min = 0, max = 3, step = 1),
    helpText("Window size must be an odd number ≥ 3. Derivative order: 0 = smoothing only, 1 = first derivative, etc.")
  )
}

smoothing_input_server <- function(id, spectra_frame) {
  moduleServer(id, function(input, output, session) {

    reactive({
      list(window = input$window,
           deriv_order = input$deriv_order
      )
    })
  })
}

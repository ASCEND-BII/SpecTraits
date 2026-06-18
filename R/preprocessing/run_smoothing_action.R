################################################################################
### Run smoothing action module (Savitzky-Golay filter)
################################################################################

run_smoothing_action_io <- function(id) {
  ns <- NS(id)
  tagList(
    p("Apply Savitzky-Golay smoothing filter to spectra"),
    actionButton(ns("run_smoothing"),
                 "Run",
                 class = "btn-primary")
  )
}

run_smoothing_action_server <- function(id, spectra_frame, smoothing_args) {
  moduleServer(id, function(input, output, session) {

    smoothed <- reactiveVal(NULL)

    observeEvent(input$run_smoothing, {

      showPageSpinner()
      on.exit(hidePageSpinner(), add = TRUE)

      df <- spectra_frame()
      req(df)

      args <- smoothing_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      window      <- args$window
      deriv_order <- args$deriv_order

      validate(need(!is.na(window) && window >= 3,       "Window size must be >= 3."))
      validate(need(window %% 2 == 1,                    "Window size must be an odd number."))
      validate(need(!is.na(deriv_order) && deriv_order >= 0, "Derivative order must be >= 0."))

      result <- spectra_smooth(df, window = window, deriv_order = deriv_order)
      smoothed(result)

    })

    return(smoothed)
  })

}

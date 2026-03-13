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

    smoothed <- eventReactive(input$run_smoothing, {

      df <- spectra_frame()
      req(df)

      args <- smoothing_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      # First column is ID
      id_col <- names(df)[1]

      window <- args$window
      deriv_order <- args$deriv_order

      # Validate parameters
      validate(need(!is.na(window) && window >= 3,
                    "Window size must be >= 3."))
      validate(need(window %% 2 == 1,
                    "Window size must be an odd number."))
      validate(need(!is.na(deriv_order) && deriv_order >= 0,
                    "Derivative order must be >= 0."))

      # Extract spectral columns (all except first ID column)
      spec_cols <- 2:ncol(df)
      spec_matrix <- as.matrix(df[, spec_cols, with = FALSE])

      # Apply Savitzky-Golay filter to each row (spectrum)
      smoothed_matrix <- t(apply(spec_matrix, 1, function(row) {
        signal::sgolayfilt(row, p = 3, n = window, m = deriv_order)
      }))

      # Create result data frame
      df_smoothed <- as.data.frame(smoothed_matrix)
      names(df_smoothed) <- names(df)[spec_cols]
      df_smoothed <- cbind(df[, 1, with = FALSE], df_smoothed)
      names(df_smoothed)[1] <- id_col

      df_smoothed

    })

    return(smoothed)
  })

}

################################################################################
### Run transformation action module
################################################################################

run_transformation_action_io <- function(id) {
  ns <- NS(id)
  tagList(
    p("Apply spectral transformation"),
    actionButton(ns("run_transformation"),
                 "Run",
                 class = "btn-primary")
  )
}

run_transformation_action_server <- function(id, spectra_frame, transformation_args) {
  moduleServer(id, function(input, output, session) {

    transformed <- reactiveVal(NULL)

    observeEvent(input$run_transformation, {

      showPageSpinner()
      on.exit(hidePageSpinner(), add = TRUE)

      df <- spectra_frame()
      req(df)

      args <- transformation_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      transform_type    <- args$transform_type
      scales            <- args$scales
      variance          <- args$variance
      deriv_window      <- args$deriv_window
      deriv_scale_order <- args$deriv_scale_order

      if (transform_type == "wavelet") {
        validate(need(!is.null(scales) && length(scales) > 0,
                      "Scales must be specified for wavelet transformation."))
        validate(need(!is.na(variance) && variance > 0, "Variance must be positive."))
      } else if (transform_type == "derivative") {
        validate(need(!is.na(deriv_window) && deriv_window >= 3, "Band window must be >= 3."))
        validate(need(deriv_window %% 2 == 1,                    "Band window must be an odd number."))
        validate(need(!is.na(deriv_scale_order) && deriv_scale_order >= 1, "Scale order must be >= 1."))
      }

      result <- spectra_transform(df,
                                   transform_type    = transform_type,
                                   scales            = scales,
                                   variance          = variance,
                                   deriv_window      = deriv_window,
                                   deriv_scale_order = deriv_scale_order)
      transformed(result)

    })

    return(transformed)
  })

}

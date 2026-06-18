################################################################################
### Run resampling action module (spectrolab FWHM-based resampling)
################################################################################

run_resample_action_io <- function(id) {
  ns <- NS(id)
  tagList(
    p("Apply FWHM-based spectral resampling"),
    actionButton(ns("run_resample"),
                 "Run",
                 class = "btn-primary")
  )
}

run_resample_action_server <- function(id, spectra_frame, resample_args) {
  moduleServer(id, function(input, output, session) {

    resampled <- reactiveVal(NULL)

    observeEvent(input$run_resample, {

      showPageSpinner()
      on.exit(hidePageSpinner(), add = TRUE)

      df <- spectra_frame()
      req(df)

      args <- resample_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      wl_start <- args$wl_start
      wl_end   <- args$wl_end
      wl_step  <- args$wl_step

      validate(need(!is.na(wl_start) && !is.na(wl_end) && !is.na(wl_step) && wl_step > 0,
                    "Check wavelength range and step (must be numeric and step > 0)."))
      validate(need(length(seq(wl_start, wl_end, by = wl_step)) >= 2,
                    "Target wavelength grid must contain at least two points."))

      result <- spectra_resample(df, wl_start = wl_start, wl_end = wl_end, wl_step = wl_step)
      resampled(result)

    })

    return(resampled)
  })

}

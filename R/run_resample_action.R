################################################################################
### Run resampling action module (spectrolab FWHM-based resampling)
################################################################################

run_resample_action_io <- function(id) {
  ns <- NS(id)
  tagList(
    p("Apply FWHM-based spectral resampling"),
    actionButton(ns("run_resample"),
                 "Run resampling",
                 class = "btn-primary")
  )
}

run_resample_action_server <- function(id, spectra_frame, resample_args) {
  moduleServer(id, function(input, output, session) {

    resampled <- eventReactive(input$run_resample, {

      df <- spectra_frame()
      req(df)

      args <- resample_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      # First column is ID (as in your predict panel)
      id_col <- names(df)[1]

      wl_start <- args$wl_start
      wl_end <- args$wl_end
      wl_step  <- args$wl_step

      validate(need(!is.na(wl_start) && !is.na(wl_end) && !is.na(wl_step) && wl_step > 0,
                    "Check wavelength range and step (they must be numeric and step > 0)."))

      new_bands <- seq(wl_start, wl_end, by = wl_step)
      validate(need(length(new_bands) >= 2, "Target wavelength grid must contain at least two points."))

      #-----------------------------------------------------------------------
      # Convert to spectrolab::spectra
      spec <- spectrolab::as_spectra(df, name_idx = 1)

      # Estimate FWHM at new bands using Gaussian model
      fwhm <- spectrolab::make_fwhm(spec, new_bands = new_bands)

      # Resample spectra to new_bands using FWHM-based Gaussian model
      spec_res <- spectrolab::resample(spec,
                                       new_bands = new_bands,
                                       fwhm = fwhm)

      # Convert back to data.frame
      df_res <- as.data.frame(spec_res,
                              fix_names = "none",
                              metadata = TRUE)

      # Ensure first column is called ID, to match your app convention
      names(df_res)[1] <- id_col

      df_res

    })

    return(resampled)
  })

}

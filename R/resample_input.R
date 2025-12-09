################################################################################
### Resampling settings input module
################################################################################

resample_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Define the target spectral spacing and range for FWHM-based resampling"),
    p("Spectra will be resampled using a Gaussian model that uses the Full Width at
      Half Maximum (FWHM) of the instrument bands (spectrolab; Meireles et al. 2022)."),
    numericInput(ns("wl_start"), "Target start wavelength (nm)", value = 400),
    numericInput(ns("wl_end"), "Target end wavelength (nm)", value = 2400),
    numericInput(ns("wl_step"), "Target step (Δλ, nm)", value = 10, min = 0.01)
  )
}

resample_input_server <- function(id, spectra_frame) {
  moduleServer(id, function(input, output, session) {

    reactive({
      list(wl_start = input$wl_start,
           wl_end   = input$wl_end,
           wl_step  = input$wl_step
      )
    })
  })
}

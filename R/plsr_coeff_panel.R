################################################################################
### PLSR module panel

plsr_coeff_ui <- function(id) {
  ns <- NS(id)
  tagList(
    coefficients_import_ui(ns("plsr_coeff_import"), "Import PLSR coeff:")
  )
}

plsr_coeff_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Import PLSR coeff
    coeff_frame <- coefficients_import_server("plsr_coeff_import",
                                              stringsAsFactors = FALSE)

  })
}


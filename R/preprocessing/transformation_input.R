################################################################################
### Spectral transformation settings input module
################################################################################

transformation_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Select a transformation to apply to the spectra"),
    selectInput(ns("transform_type"), "Transformation type",
                choices = c("Vector normalization (2-Norm)" = "norm",
                           "Summed-wavelet spectra" = "wavelet",
                           "Derivative" = "derivative")),

    # Conditional panels for different transformation types
    conditionalPanel(
      condition = sprintf("input['%s'] == 'wavelet'", ns("transform_type")),
      textInput(ns("scales"), "Scales (comma-separated, e.g., 1,2,3)", value = "1,2,3"),
      numericInput(ns("variance"), "Variance", value = 1, min = 0.01, step = 0.1),
      helpText("Summed-wavelet spectra using continuous wavelet transform (CWT package)")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'derivative'", ns("transform_type")),
      numericInput(ns("deriv_window"), "Band window", value = 11, min = 3, step = 2),
      numericInput(ns("deriv_scale_order"), "Scale order", value = 1, min = 1, max = 3, step = 1),
      helpText("Derivative transformation with specified band window and scale order")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'norm'", ns("transform_type")),
      helpText("Vector normalization using Euclidean norm (2-Norm). No parameters needed.")
    )
  )
}

transformation_input_server <- function(id, spectra_frame) {
  moduleServer(id, function(input, output, session) {

    reactive({

      # Parse scales for wavelet
      scales_val <- NULL
      if (input$transform_type == "wavelet" && !is.null(input$scales)) {
        tryCatch({
          scales_val <- as.numeric(strsplit(as.character(input$scales), ",")[[1]])
        }, error = function(e) {
          scales_val <<- c(1, 2, 3)
        })
      }

      list(
        transform_type = input$transform_type,
        scales = scales_val,
        variance = input$variance,
        deriv_window = input$deriv_window,
        deriv_scale_order = input$deriv_scale_order
      )
    })
  })
}

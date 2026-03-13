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

    transformed <- eventReactive(input$run_transformation, {

      df <- spectra_frame()
      req(df)

      args <- transformation_args()

      validate(need(ncol(df) >= 2, "Spectra file must contain an ID column plus wavelength columns."))

      # First column is ID
      id_col <- names(df)[1]

      # Extract spectral columns (all except first ID column)
      spec_cols <- 2:ncol(df)
      spec_matrix <- as.matrix(df[, spec_cols, with = FALSE])

      transform_type <- args$transform_type

      #-----------------------------------------------------------------------
      # Apply transformation based on type
      #-----------------------------------------------------------------------

      if (transform_type == "norm") {
        # Vector normalization using Euclidean norm (2-Norm)
        transformed_matrix <- t(apply(spec_matrix, 1, function(row) {
          row / sqrt(sum(row^2))
        }))

      } else if (transform_type == "wavelet") {
        # Summed-wavelet spectra using CWT
        scales <- args$scales
        variance <- args$variance

        validate(need(!is.null(scales) && length(scales) > 0,
                      "Scales must be specified for wavelet transformation."))
        validate(need(!is.na(variance) && variance > 0,
                      "Variance must be positive."))

        # Apply CWT directly to the matrix (rows = samples, columns = bands)
        transformed_matrix <- CWT::cwt(t = spec_matrix,
                                       scales = scales,
                                       variance = variance,
                                       summed_wavelet = TRUE,
                                       threads = 1)

      } else if (transform_type == "derivative") {
        # Derivative transformation
        window <- args$deriv_window
        scale_order <- args$deriv_scale_order

        validate(need(!is.na(window) && window >= 3,
                      "Band window must be >= 3."))
        validate(need(window %% 2 == 1,
                      "Band window must be an odd number."))
        validate(need(!is.na(scale_order) && scale_order >= 1,
                      "Scale order must be >= 1."))

        # Apply Savitzky-Golay derivative filter
        transformed_matrix <- t(apply(spec_matrix, 1, function(row) {
          signal::sgolayfilt(row, p = 3, n = window, m = scale_order)
        }))

      } else {
        stop("Unknown transformation type")
      }

      #-----------------------------------------------------------------------
      # Create result data frame
      #-----------------------------------------------------------------------
      df_transformed <- as.data.frame(transformed_matrix)
      names(df_transformed) <- names(df)[spec_cols]
      df_transformed <- cbind(df[, 1, with = FALSE], df_transformed)
      names(df_transformed)[1] <- id_col

      df_transformed

    })

    return(transformed)
  })

}

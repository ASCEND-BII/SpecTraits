################################################################################
### Export processed spectra module (generic export for all preprocessing types)
################################################################################

processed_export_ui <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
    p("Download the processed spectra as a .csv file."),
    downloadButton(ns("download_processed"), label,
                   class = "btn-success",
                   style = "background-color: #005F5F; border-color: #005F5F; color: white;")
  )
}

processed_export_server <- function(id, data, filename_prefix = NULL) {
  moduleServer(id, function(input, output, session) {

    output$download_processed <- downloadHandler(
      filename = function() {
        # Use reactive prefix if provided, otherwise default
        prefix <- if (is.reactive(filename_prefix)) {
          filename_prefix()
        } else if (!is.null(filename_prefix)) {
          filename_prefix
        } else {
          "processed_spectra"
        }
        paste0(prefix, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- data()
        req(df)
        fwrite(df, file, row.names = FALSE)
      }
    )

  })
}

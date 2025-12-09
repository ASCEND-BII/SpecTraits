################################################################################
### Export resampled spectra module
################################################################################

resampled_export_ui <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
    p("Download the resampled spectra as a .csv file."),
    downloadButton(ns("download_resampled"), label)
  )
}

resampled_export_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$download_resampled <- downloadHandler(
      filename = function() {
        paste0("resampled_spectra_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- data()
        req(df)
        fwrite(df, file, row.names = FALSE)
      }
    )

  })
}

################################################################################
##### Build export
################################################################################

################################################################################
#UI
build_export_ui <- function(export) {
  ns <- NS(export)
  tagList(
    h4("Export model"),
    downloadButton(ns("download_all"), "Download results")
  )
}

#-------------------------------------------------------------------------------
#Server
build_export_server <- function(export,
                                trait_selector,
                                press_frame,
                                final_PLSR,
                                results_predict) {

  moduleServer(export, function(input, output, session) {

    output$download_all <- downloadHandler(

      filename = function() {
        paste0("model_", trait_selector, "_", Sys.Date(), ".zip")
      },

      content = function(zipfile) {

        # Temporary files for CSVs
        tmpdir <- tempdir()
        file1 <- file.path(tmpdir, paste0("RMSEP_", trait_selector, ".csv"))
        file2 <- file.path(tmpdir, paste0("PLSR-coefficients_", trait_selector, ".csv"))
        file3 <- file.path(tmpdir, paste0("VIP_", trait_selector, ".csv"))
        file4 <- file.path(tmpdir, paste0("Predicted_", trait_selector, ".csv"))

        # Write CSVs
        data.table::fwrite(press_frame()$rmsep, file1)
        data.table::fwrite(final_PLSR()$coefficients, file2)
        data.table::fwrite(final_PLSR()$vip, file3)
        data.table::fwrite(results_predict(), file4)

        # Create ZIP archive
        zip(zipfile, files = c(file1, file2, file3, file4), flags = "-j")
      },

      contentType = "application/zip"
    )
  })
}



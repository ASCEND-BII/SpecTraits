################################################################################
##### Build export
################################################################################

################################################################################
#UI
build_export_ui <- function(export) {
  ns <- NS(export)
  tagList(
    h4("Export model"),
    downloadButton(ns("downloadZip"), "Download ZIP")
  )
}

#-------------------------------------------------------------------------------
#Server
build_export_server <- function(export,
                                trait_selector,
                                build_import_figure,
                                split_method,
                                split_action_figure,
                                press_method,
                                press_frame,
                                press_action_figure,
                                final_method,
                                final_PLSR,
                                coefficients_figure,
                                results_predict,
                                perf_train_figure,
                                perf_test_figure) {

  moduleServer(export, function(input, output, session) {
    output$downloadZip <- downloadHandler(

      filename = function() {
        paste0(trait_selector(), "_", Sys.Date(), ".zip")
      },

      content = function(filename) {

        # Spinner
        showPageSpinner()
        on.exit(hidePageSpinner(), add = TRUE)

        # Temporary files for CSVs
        tmpdir <- tempdir()

        # Build import spectra figure
        build_import_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_spectra", ".jpeg"))
        jpeg(build_import_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- build_import_figure$spectra_figure()
        print(plot_obj)
        dev.off()

        # Build import histogram distribution figure
        build_import_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_histogram-distribution", ".jpeg"))
        jpeg(build_import_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- build_import_figure$trait_figure()
        print(plot_obj)
        dev.off()

        # Split spectra
        split_action_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "spectra-split", ".jpeg"))
        jpeg(split_action_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- split_action_figure$spectra_split_figure()
        print(plot_obj)
        dev.off()

        # Split trait
        split_action_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "trait-distribution-split", ".jpeg"))
        jpeg(split_action_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- split_action_figure$trait_split_figure()
        print(plot_obj)
        dev.off()

        # Press
        press_frame_file <- file.path(tmpdir, paste0(trait_selector(), "_RMSEP", ".csv"))
        fwrite(press_frame()$rmsep, press_frame_file)

        press_action_figure_file <- file.path(tmpdir, paste0(trait_selector(), "_RMSEP-components", ".jpeg"))
        jpeg(press_action_figure_file, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- press_action_figure()
        print(plot_obj)
        dev.off()

        # Final model
        final_PLSR_file1 <- file.path(tmpdir, paste0(trait_selector(), "_coefficients", ".csv"))
        fwrite(final_PLSR()$coefficients, final_PLSR_file1)

        final_PLSR_file2 <- file.path(tmpdir, paste0(trait_selector(), "_vip", ".csv"))
        fwrite(final_PLSR()$vip, final_PLSR_file2)

        coefficients_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_coefficients", ".jpeg"))
        jpeg(coefficients_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- coefficients_figure$coefficients()
        print(plot_obj)
        dev.off()

        vip_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_vip", ".jpeg"))
        jpeg(vip_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- coefficients_figure$vip()
        print(plot_obj)
        dev.off()

        # Observed predicted frame
        results_predict_file <- file.path(tmpdir, paste0(trait_selector(), "_observed-predicted", ".csv"))
        fwrite(results_predict(), results_predict_file)

        # Performance train
        perf_train_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_training_observed-predicted", ".jpeg"))
        jpeg(perf_train_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$obser_pred()
        print(plot_obj)
        dev.off()

        perf_train_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_training_histogram", ".jpeg"))
        jpeg(perf_train_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$histogram()
        print(plot_obj)
        dev.off()

        perf_train_figure_file3 <- file.path(tmpdir, paste0(trait_selector(), "_training_residuals", ".jpeg"))
        jpeg(perf_train_figure_file3, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$residuals()
        print(plot_obj)
        dev.off()

        perf_train_figure_file4 <- file.path(tmpdir, paste0(trait_selector(), "_training_performance", ".csv"))
        fwrite(perf_train_figure$performance(), perf_train_figure_file4)

        # Performance testing
        perf_test_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_testing_observed-predicted", ".jpeg"))
        jpeg(perf_test_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$obser_pred()
        print(plot_obj)
        dev.off()

        perf_test_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_testing_histogram", ".jpeg"))
        jpeg(perf_test_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$histogram()
        print(plot_obj)
        dev.off()

        perf_test_figure_file3 <- file.path(tmpdir, paste0(trait_selector(), "_testing_residuals", ".jpeg"))
        jpeg(perf_test_figure_file3, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$residuals()
        print(plot_obj)
        dev.off()

        perf_test_figure_file4 <- file.path(tmpdir, paste0(trait_selector(), "_testing_performance", ".csv"))
        fwrite(perf_test_figure$performance(), perf_test_figure_file4)

        zipr(zipfile = filename, files = c(build_import_figure_file1,
                                           build_import_figure_file2,
                                           split_action_figure_file1,
                                           split_action_figure_file2,
                                           press_frame_file,
                                           press_action_figure_file,
                                           final_PLSR_file1,
                                           final_PLSR_file2,
                                           coefficients_figure_file1,
                                           vip_figure_file2,
                                           results_predict_file,
                                           perf_train_figure_file1,
                                           perf_train_figure_file2,
                                           perf_train_figure_file3,
                                           perf_train_figure_file4,
                                           perf_test_figure_file1,
                                           perf_test_figure_file2,
                                           perf_test_figure_file3,
                                           perf_test_figure_file4
                                           ))

      },

      contentType = "application/zip"

    )
  })
}


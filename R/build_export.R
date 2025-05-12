################################################################################
##### Build export
################################################################################

################################################################################
#UI
build_export_ui <- function(export) {
  ns <- NS(export)
  tagList(
    h4("Export model"),
    downloadButton(ns("download_all"), "Download ZIP")
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

    output$download_all <- downloadHandler(

      filename = function() {
        paste0("model_", Sys.Date(), ".zip") # trait_selector(), "_",
      },

      content = function(zipfile) {

        # Temporary files for CSVs
        tmpdir <- tempdir()
        # trait_selector_file <- file.path(tmpdir, paste0("RMSEP_", trait_selector(), ".csv"))

        # Build import figure
        build_import_figure_file1 <- file.path(tmpdir, paste0("spectra_", trait_selector(), ".jpeg"))
        build_import_figure_file2 <- file.path(tmpdir, paste0("histogram_", trait_selector(), ".jpeg"))
        # ggsave(build_import_figure_file1, plot = build_import_figure()$spectra_figure, width = 90, height = 60, units = "mm", dpi = 300)
        # ggsave(build_import_figure_file2, plot = build_import_figure()$trait_figure, width = 90, height = 60, units = "mm", dpi = 300)

        # Split
        # split_method_file <- file.path(tmpdir, paste0("RMSEP_", trait_selector(), ".csv"))
        split_action_figure_file1 <- file.path(tmpdir, paste0("spectra-split_", trait_selector(), ".jpeg"))
        split_action_figure_file2 <- file.path(tmpdir, paste0("trait-split_", trait_selector(), ".jpeg"))
        ggsave(split_action_figure_file1, plot = split_action_figure()$spectra_split_figure, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(split_action_figure_file2, plot = split_action_figure()$trait_split_figure, width = 90, height = 60, units = "mm", dpi = 300)

        # Press
        # press_method_file <- file.path(tmpdir, paste0("RMSEP_", trait_selector(), ".csv"))
        press_frame_file <- file.path(tmpdir, paste0("RMSEP_", trait_selector(), ".csv"))
        press_action_figure_file <- file.path(tmpdir, paste0("RMSEP-components_", trait_selector(), ".jpeg"))
        fwrite(press_frame()$rmsep, press_frame_file)
        ggsave(press_action_figure_file, plot = press_action_figure, width = 90, height = 60, units = "mm", dpi = 300)

        # Final model
        # final_method_file <- file.path(tmpdir, paste0("RMSEP_", trait_selector(), ".csv"))
        final_PLSR_file1 <- file.path(tmpdir, paste0("coefficients_", trait_selector(), ".csv"))
        final_PLSR_file2 <- file.path(tmpdir, paste0("vip_", trait_selector(), ".csv"))
        coefficients_figure_file1 <- file.path(tmpdir, paste0("coefficients_", trait_selector(), ".jpeg"))
        vip_figure_file2 <- file.path(tmpdir, paste0("vip_", trait_selector(), ".jpeg"))
        fwrite(final_PLSR()$coefficients, final_PLSR_file1)
        fwrite(final_PLSR()$vip, final_PLSR_file2)
        ggsave(coefficients_figure_file1, plot = coefficients_figure()$coefficients, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(vip_figure_file2, plot = coefficients_figure()$vip, width = 90, height = 60, units = "mm", dpi = 300)

        # Observed predicted frame
        results_predict_file <- file.path(tmpdir, paste0("observed-predicted_", trait_selector(), ".csv"))
        fwrite(results_predict(), results_predict_file)

        # Performance train
        perf_train_figure_file1 <- file.path(tmpdir, paste0("training_observed-predicted_", trait_selector(), ".jpeg"))
        perf_train_figure_file2 <- file.path(tmpdir, paste0("training_histogram_", trait_selector(), ".jpeg"))
        perf_train_figure_file3 <- file.path(tmpdir, paste0("training_residuals_", trait_selector(), ".jpeg"))
        perf_train_figure_file4 <- file.path(tmpdir, paste0("training_performance_", trait_selector(), ".csv"))
        ggsave(perf_train_figure_file1, plot = perf_train_figure()$obser_pred, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(perf_train_figure_file2, plot = perf_train_figure()$histogram, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(perf_train_figure_file3, plot = perf_train_figure()$residuals, width = 90, height = 60, units = "mm", dpi = 300)
        fwrite(perf_train_figure()$performance, perf_train_figure_file4)

        # Performance testing
        perf_test_figure_file1 <- file.path(tmpdir, paste0("testing_observed-predicted_", trait_selector(), ".jpeg"))
        perf_test_figure_file2 <- file.path(tmpdir, paste0("testing_histogram_", trait_selector(), ".jpeg"))
        perf_test_figure_file3 <- file.path(tmpdir, paste0("testing_residuals_", trait_selector(), ".jpeg"))
        perf_test_figure_file4 <- file.path(tmpdir, paste0("testing_performance_", trait_selector(), ".csv"))
        ggsave(perf_test_figure_file1, plot = perf_test_figure()$obser_pred, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(perf_test_figure_file2, plot = perf_test_figure()$histogram, width = 90, height = 60, units = "mm", dpi = 300)
        ggsave(perf_test_figure_file3, plot = perf_test_figure()$residuals, width = 90, height = 60, units = "mm", dpi = 300)
        fwrite(perf_test_figure()$performance, perf_test_figure_file4)

        # Create ZIP archive
        zip(zipfile, files = c(build_import_figure_file1, build_import_figure_file2,
                               split_action_figure_file1, split_action_figure_file2,
                               press_frame_file, press_action_figure_file,
                               final_PLSR_file1, final_PLSR_file2, coefficients_figure_file1, vip_figure_file2,
                               results_predict_file,
                               perf_train_figure_file1, perf_train_figure_file2, perf_train_figure_file3, perf_train_figure_file4,
                               perf_test_figure_file1, perf_test_figure_file2, perf_test_figure_file3, perf_test_figure_file4),
            flags = "-j")
      },

      contentType = "application/zip"
    )
  })
}

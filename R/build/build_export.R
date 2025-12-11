################################################################################
##### Build export
################################################################################

################################################################################
#UI
build_export_ui <- function(export) {
  ns <- NS(export)
  tagList(
    h4(" "),

    br(),

    # Report options
    checkboxInput(ns("include_report"),
                  "Include report in ZIP",
                  value = TRUE),

    textInput(ns("report_author"),
              "Report Author:",
              value = "SpecTraits User",
              width = "100%"),

    selectInput(ns("report_format"),
                "Report Format:",
                choices = c("PDF" = "pdf", "HTML" = "html"),
                selected = "pdf",
                width = "100%"),

    br(),

    downloadButton(ns("downloadZip"), "Download ZIP", class = "btn-success")
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

        # List to store all file paths
        all_files <- c()

        # Build import spectra figure
        build_import_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_spectra", ".jpeg"))
        jpeg(build_import_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- build_import_figure$spectra_figure()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, build_import_figure_file1)

        # Build import histogram distribution figure
        build_import_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_histogram-distribution", ".jpeg"))
        jpeg(build_import_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- build_import_figure$trait_figure()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, build_import_figure_file2)

        # Split spectra
        split_action_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_spectra-split", ".jpeg"))
        jpeg(split_action_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- split_action_figure$spectra_split_figure()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, split_action_figure_file1)

        # Split trait
        split_action_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_trait-distribution-split", ".jpeg"))
        jpeg(split_action_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- split_action_figure$trait_split_figure()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, split_action_figure_file2)

        # Press
        press_frame_file <- file.path(tmpdir, paste0(trait_selector(), "_RMSEP", ".csv"))
        fwrite(press_frame()$rmsep, press_frame_file)
        all_files <- c(all_files, press_frame_file)

        press_action_figure_file <- file.path(tmpdir, paste0(trait_selector(), "_RMSEP-components", ".jpeg"))
        jpeg(press_action_figure_file, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- press_action_figure()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, press_action_figure_file)

        # Final model
        final_PLSR_file1 <- file.path(tmpdir, paste0(trait_selector(), "_coefficients", ".csv"))
        fwrite(final_PLSR()$coefficients, final_PLSR_file1)
        all_files <- c(all_files, final_PLSR_file1)

        final_PLSR_file2 <- file.path(tmpdir, paste0(trait_selector(), "_vip", ".csv"))
        fwrite(final_PLSR()$vip, final_PLSR_file2)
        all_files <- c(all_files, final_PLSR_file2)

        coefficients_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_coefficients", ".jpeg"))
        jpeg(coefficients_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- coefficients_figure$coefficients()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, coefficients_figure_file1)

        vip_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_vip", ".jpeg"))
        jpeg(vip_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- coefficients_figure$vip()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, vip_figure_file2)

        # Observed predicted frame
        results_predict_file <- file.path(tmpdir, paste0(trait_selector(), "_observed-predicted", ".csv"))
        fwrite(results_predict(), results_predict_file)
        all_files <- c(all_files, results_predict_file)

        # Performance train
        perf_train_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_training_observed-predicted", ".jpeg"))
        jpeg(perf_train_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$obser_pred()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_train_figure_file1)

        perf_train_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_training_histogram", ".jpeg"))
        jpeg(perf_train_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$histogram()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_train_figure_file2)

        perf_train_figure_file3 <- file.path(tmpdir, paste0(trait_selector(), "_training_residuals", ".jpeg"))
        jpeg(perf_train_figure_file3, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_train_figure$residuals()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_train_figure_file3)

        perf_train_figure_file4 <- file.path(tmpdir, paste0(trait_selector(), "_training_performance", ".csv"))
        fwrite(perf_train_figure$performance(), perf_train_figure_file4)
        all_files <- c(all_files, perf_train_figure_file4)

        # Performance testing
        perf_test_figure_file1 <- file.path(tmpdir, paste0(trait_selector(), "_testing_observed-predicted", ".jpeg"))
        jpeg(perf_test_figure_file1, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$obser_pred()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_test_figure_file1)

        perf_test_figure_file2 <- file.path(tmpdir, paste0(trait_selector(), "_testing_histogram", ".jpeg"))
        jpeg(perf_test_figure_file2, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$histogram()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_test_figure_file2)

        perf_test_figure_file3 <- file.path(tmpdir, paste0(trait_selector(), "_testing_residuals", ".jpeg"))
        jpeg(perf_test_figure_file3, width = 150, height = 90, units = "mm", res = 300)
        plot_obj <- perf_test_figure$residuals()
        print(plot_obj)
        dev.off()
        all_files <- c(all_files, perf_test_figure_file3)

        perf_test_figure_file4 <- file.path(tmpdir, paste0(trait_selector(), "_testing_performance", ".csv"))
        fwrite(perf_test_figure$performance(), perf_test_figure_file4)
        all_files <- c(all_files, perf_test_figure_file4)

        # Generate Quarto Report
        if (input$include_report) {
          tryCatch({

            showNotification("Generating report...", type = "message", id = "report_gen")

            # Prepare parameters for the report
            params <- list(
              author = input$report_author,
              date = Sys.Date(),
              trait_selector = trait_selector(),
              split_method = split_method(),
              press_method = press_method(),
              press_frame = press_frame(),
              final_method = final_method(),
              final_PLSR = final_PLSR(),
              results_predict = results_predict(),
              build_import_figure = build_import_figure,
              split_action_figure = split_action_figure,
              press_action_figure = press_action_figure,
              coefficients_figure = coefficients_figure,
              perf_train_figure = perf_train_figure,
              perf_test_figure = perf_test_figure
            )

            # Get template path
            template_path <- here::here("R/build/plsr_report_template.qmd")

            if (file.exists(template_path)) {

              # Copy template to temp directory
              temp_qmd <- file.path(tmpdir, "report.qmd")
              file.copy(template_path, temp_qmd, overwrite = TRUE)

              # Render the report
              quarto::quarto_render(
                input = temp_qmd,
                output_format = input$report_format,
                execute_params = params,
                output_file = paste0(trait_selector(), "_report.", input$report_format)
              )

              # Add rendered report to files list
              report_file <- file.path(tmpdir, paste0(trait_selector(), "_report.", input$report_format))

              if (file.exists(report_file)) {
                all_files <- c(all_files, report_file)
                removeNotification("report_gen")
                showNotification("Report generated successfully!", type = "message", duration = 3)
              }

            } else {
              showNotification(
                "Report template not found. Skipping report generation.",
                type = "warning",
                duration = 5
              )
            }

          }, error = function(e) {
            removeNotification("report_gen")
            showNotification(
              paste("Report generation failed:", e$message),
              type = "warning",
              duration = 5
            )
          })
        }

        # Create ZIP file with all files
        zipr(zipfile = filename, files = all_files)

      },

      contentType = "application/zip"

    )
  })
}


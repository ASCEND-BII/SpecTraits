preprocessing_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<h3 style='color:#005F5F; font-weight:bold;'>Pre-process leaf spectra (spectral resampling)</h3>"),
    br(" "),
    fluidRow(

      # Left column: workflow steps
      column(3,
             p("Upload a spectra file and perform spectral resampling:"),
             tags$ul(
               tags$li("Resample from higher to lower spectral resolution")
             ),
             p("The spectra file must contain wavelengths (nm) as columns and
               samples as rows, with a first column named ID."),

             br(""),

             wellPanel(
               h4("Step 1 - Import spectra file"),
               spectra_import_ui(ns("spectra_import"), "Choose file:")
             ),
             br(),

             wellPanel(
               h4("Step 2 - Define resampling grid"),
               resample_input_ui(ns("resample_input"))
             ),
             br(),

             wellPanel(
               h4("Step 3 - Run resampling"),
               run_resample_action_io(ns("resample_run"))
             ),
             br(),

             wellPanel(
               h4("Step 4 - Export resampled spectra"),
               resampled_export_ui(ns("resample_export"), "Download resampled spectra"),
               tags$hr()
             )
      ),

      # Right column: outputs
      column(9,
             tabsetPanel(type = "tabs",
                         # Original spectra table
                         tabPanel("Original spectra",
                                  DT::dataTableOutput(ns("original_df"))),

                         # Resampled spectra table
                         tabPanel("Resampled spectra",
                                  DT::dataTableOutput(ns("resampled_df")))
             )
      )
    )
  )
}

#-------------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------------

preprocessing_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Step 1 - Import spectra --------------------------------------------------
    spectra_frame <- spectra_import_server("spectra_import")

    # Show original spectra table
    output$original_df <- DT::renderDataTable({req(spectra_frame())
      DT::datatable(spectra_frame(),
                    rownames = FALSE,
                    options = list(scrollX = TRUE,
                                   pageLength = 10)
                    )})

    # Step 2 - Resampling settings ---------------------------------------------
    resample_args <- resample_input_server("resample_input",
                                           spectra_frame = spectra_frame)

    # Step 3 - Run resampling --------------------------------------------------
    resampled_frame <- run_resample_action_server("resample_run",
                                                  spectra_frame = spectra_frame,
                                                  resample_args = resample_args)

    # Show resampled spectra table
    output$resampled_df <- DT::renderDataTable({
      req(resampled_frame())
      DT::datatable(resampled_frame(),
                    rownames = FALSE,
                    options = list(scrollX = TRUE,
                                   pageLength = 10)
                    )})

    # Step 4 - Export resampled spectra ---------------------------------------
    resampled_export_server("resample_export",
                            data = resampled_frame)

  })
}

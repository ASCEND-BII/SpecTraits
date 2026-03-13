preprocessing_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<h3 style='color:#005F5F; font-weight:bold;'>Pre-process leaf spectra</h3>"),
    br(" "),
    fluidRow(

      # Left column: workflow steps
      column(3,
             p("Upload a spectra file and select a preprocessing method to apply."),
             p("The spectra file (.csv) must contain wavelengths (nm) as columns and
               samples as rows, with a first column named ID."),
             br(""),

             wellPanel(
               h4("Step 1 - Import spectra file"),
               spectra_import_ui(ns("spectra_import"), "Choose file:")
             ),
             br(),

             wellPanel(
               h4("Step 2 - Select preprocessing method"),
               radioButtons(ns("preprocess_method"),
                           "Preprocessing method:",
                           choices = c("Spectral resampling" = "resample",
                                      "Spectral smoothing" = "smoothing",
                                      "Spectral transformation" = "transformation"),
                           selected = "resample")
             ),
             br(),

             wellPanel(
               h4("Step 3 - Configure settings"),

               # Conditional panels for each preprocessing method
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'resample'", ns("preprocess_method")),
                 resample_input_ui(ns("resample_input"))
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'smoothing'", ns("preprocess_method")),
                 smoothing_input_ui(ns("smoothing_input"))
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'transformation'", ns("preprocess_method")),
                 transformation_input_ui(ns("transformation_input"))
               )
             ),
             br(),

             wellPanel(
               h4("Step 4 - Run preprocessing"),

               # Conditional action buttons for each method
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'resample'", ns("preprocess_method")),
                 run_resample_action_io(ns("resample_run"))
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'smoothing'", ns("preprocess_method")),
                 run_smoothing_action_io(ns("smoothing_run"))
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'transformation'", ns("preprocess_method")),
                 run_transformation_action_io(ns("transformation_run"))
               )
             ),
             br(),

             wellPanel(
               h4("Step 5 - Export processed spectra"),
               processed_export_ui(ns("processed_export"), "Download"),
               tags$hr()
             )
      ),

      # Right column: outputs
      column(9,
             tabsetPanel(type = "tabs",
                         # Original spectra table
                         tabPanel("Original spectra",
                                  DT::dataTableOutput(ns("original_df"))),

                         # Processed spectra table
                         tabPanel("Processed spectra",
                                  DT::dataTableOutput(ns("processed_df"))),

                         # Visualization comparison
                         tabPanel("Visualization",
                                  br(),
                                  preprocessing_plot_ui(ns("preprocessing_plot")))
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

    # Step 2 & 3 - Settings for each preprocessing method ---------------------

    # Resampling settings
    resample_args <- resample_input_server("resample_input",
                                           spectra_frame = spectra_frame)

    # Smoothing settings
    smoothing_args <- smoothing_input_server("smoothing_input",
                                            spectra_frame = spectra_frame)

    # Transformation settings
    transformation_args <- transformation_input_server("transformation_input",
                                                      spectra_frame = spectra_frame)

    # Step 4 - Run preprocessing -----------------------------------------------

    # Run resampling
    resampled_frame <- run_resample_action_server("resample_run",
                                                  spectra_frame = spectra_frame,
                                                  resample_args = resample_args)

    # Run smoothing
    smoothed_frame <- run_smoothing_action_server("smoothing_run",
                                                  spectra_frame = spectra_frame,
                                                  smoothing_args = smoothing_args)

    # Run transformation
    transformed_frame <- run_transformation_action_server("transformation_run",
                                                          spectra_frame = spectra_frame,
                                                          transformation_args = transformation_args)

    # Combine results based on selected method
    processed_frame <- reactive({
      if (input$preprocess_method == "resample") {
        return(resampled_frame())
      } else if (input$preprocess_method == "smoothing") {
        return(smoothed_frame())
      } else if (input$preprocess_method == "transformation") {
        return(transformed_frame())
      }
      return(NULL)
    })

    # Show processed spectra table
    output$processed_df <- DT::renderDataTable({
      req(processed_frame())
      DT::datatable(processed_frame(),
                    rownames = FALSE,
                    options = list(scrollX = TRUE,
                                   pageLength = 10)
                    )})

    # Step 5 - Export processed spectra ---------------------------------------
    filename_prefix <- reactive({
      if (input$preprocess_method == "resample") {
        return("resampled_spectra")
      } else if (input$preprocess_method == "smoothing") {
        return("smoothed_spectra")
      } else if (input$preprocess_method == "transformation") {
        return("transformed_spectra")
      }
      return("processed_spectra")
    })

    processed_export_server("processed_export",
                           data = processed_frame,
                           filename_prefix = filename_prefix)

    # Visualization - Plot comparison ------------------------------------------
    preprocessing_plot_server("preprocessing_plot",
                             original_spectra = spectra_frame,
                             processed_spectra = processed_frame)

  })
}

################################################################################
### Predict panel

predict_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<h3 style='color:#005F5F; font-weight:bold;'>Predict leaf traits using leaf spectra</h3>"),
    br(" "),
    fluidRow(

      #Upload panel
      column(3,
             p("Predict leaf traits by uploading a .csv file that contains leaf spectra."),
             p("The spectra file most contain wavelengths (nm) as columns and samples as rows, a first column should be named ID."),
             p("SpecTraits provides two ways for predicting leaf traits: i) by importing PLSR coefficients and ii) by using radiative transfer modells (time consuming)."),
             p("Predicted traits can be validate uploading a .csv file that contains columns of leaf traits and the exact number of samples as the spectra file."),
             p("An example of files containing leaf spectra and traits can be downloaded ",
               a("here", href = "https://github.com/ASCEND-BII/SpecTraits/tree/main/inst/extdata", target = "_blank")),
             br(""),

             wellPanel(
               h4("Step 1 - Import spectra file"),
               spectra_import_ui(ns("spectra_import"), "Choose file:")),
             br(),

             wellPanel(
               h4("Step 2 - Select method"),
               method_input_ui(ns("method"))),
             br(),

             wellPanel(
               h4("Step 3 - Apply method"),
               run_action_io(ns("run"))),
             br(),

             wellPanel(
               h4("Step 4 - External validation (optional)"),
               traits_import_ui(ns("traits_import"), "Choose file:"),
               info_frame_ui(ns("frame_info")),
               tags$hr()),
             br(),

             wellPanel(
               h4("Step 5 - Export predicted traits"),
               traits_export_ui(ns("traits_export"), "Download predicted traits"),
               tags$hr())

      ),

      #Out and visualization panel
      column(9,
             tabsetPanel(type = "tabs",

                         #Plot spectra
                         tabPanel("Plot spectra",
                                  spectra_plot_ui(ns("spectra_figure"))),

                         #Plot predicted leaf traits
                         tabPanel("Predicted leaf trait",
                                  predicted_plot_ui(ns("predicted_figure"))),

                         #Validation file
                         tabPanel("External validation file",
                                  DT::dataTableOutput(ns("traits_df"))),

                         #Validate prediction
                         tabPanel("Leaf trait validation",
                                  validation_plot_ui(ns("validation_figure")))

                         #Summary report for predicted leaf traits
                         #tabPanel("Summary",
                         #          DT::dataTableOutput("coeff_df"))
             )
      )
    )
  )
}

# Predictions panel module Server logic
predict_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Spectra to import (Step 1) -----------------------------------------------

    # Import file of spectra
    spectra_frame <- spectra_import_server("spectra_import")

    # Return plot spectra after upload
    callModule(spectra_plot_server,
               "spectra_figure",
               data = spectra_frame)

    # Select method (Step 2) ---------------------------------------------------

    # Define method
    method_frame <- method_input_server("method")

    # # Optional: reactively trigger side effects #### THIS CAN BE REMOVE
    # observeEvent(method_frame(), {
    #   cat("[INFO] Method selected:", method_frame()$method, "\n")
    #   if (method_frame()$method == "pls") {
    #     print(head(method_frame()$value))
    #   } else {
    #     cat("[INFO] RTM Value:", method_frame()$value, "\n")
    #   }
    # })

    # Apply method (Step 3) ----------------------------------------------------

    # Apply method after definition
    predicted_frame <- run_action_server("run",
                                         method = method_frame()$method,
                                         spectra_frame = spectra_frame(),
                                         values = method_frame()$value)

    # Return predicted plot
    predicted_plot_server("predicted_figure",
                          data = predicted_frame,
                          method = method_frame()$method)

    # Import traits frame (Step 4) ---------------------------------------------

    # Import validation file
    traits_frame <- traits_import_server("traits_import")

    # Validation input frame
    output$traits_df <- DT::renderDataTable(DT::datatable(
      traits_frame(),
      options = list(rowCallback = DT::JS(
        'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
      ))
    ))

    # Select variable for validation
    validation_trait <- info_frame_server("frame_info", traits_frame)

    # Plot validation file
    validation_plot_server("validation_figure",
                           observed = traits_frame,
                           predicted = predicted_frame,
                           variable = validation_trait$observed,
                           method = method_frame()$method)

    # Export predictions (Step 4) ----------------------------------------------

    # Export predicted traits
    callModule(traits_export_server,
               "traits_export",
               data = predicted_frame)

  })
}

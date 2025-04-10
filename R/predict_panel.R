################################################################################
### Predict panel

predict_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
           h2("SpecTraits"),
           p(""),
           h3("Predicting leaf traits using leaf spectra"),
           br(""),
           fluidRow(

             #Upload panel
             column(3,
                    p("You can predict leaf traits by uploading a .csv file that contains leaf spectra."),
                    p("The spectra file most contain wavelengths as columns and samples as rows, a first column should be named ID."),
                    p("We provide three ways for predicting leaf traits: i) using published PLSR coefficients, ii) using radiative transfer modells, and iii) using your own PLSR coefficients"),
                    p("You can also validate the predicted traits uploading a .csv file that contains traits data"),
                    HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                    br(""),

                    wellPanel(
                      h4("Step 1 - Import files"),
                      spectra_import_ui(ns("spectra_import"), "Choose spectra:")),

                    wellPanel(
                      h4("Step 2 - Select an approach for predicting leaf traits"),
                      selectInput(ns("selection"), "Choose an approach:",
                                    choices = c("PLSR coefficients",
                                                "Radiative Transfer Models")),
                      uiOutput(ns("prediction_module_ui"))
                      ),

                    wellPanel(
                      h4("Step 3 - Apply approach"),
                      actionButton(ns("run"), "Run")
                    ),

                    wellPanel(
                      h4("Step 4 - External validation (optional)"),
                      traits_import_ui(ns("traits_import"), "Choose file:"),
                      info_frame_ui(ns("frame_info")),
                      tags$hr()),

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
  )
}

# Predictions panel module Server logic
predict_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##############################################################################
    ### PLSR coefficients and RTM

    output$prediction_module_ui <- renderUI({
      req(input$selection)
      if (input$selection == "PLSR coefficients") {
        plsr_coeff_ui(ns("plsr"))
      } else {
        rtm_coeff_ui(ns("rtm"))
      }
    })

    observe({
      req(input$selection)
      if (input$selection == "PLSR coefficients") {
        coefficients <- plsr_coeff_server("main-coeff")
      } else {
        rtm_coeff_server("main-actions")
      }
    })

    ##############################################################################
    ### Frames

    # Spectra to import
    spectra_frame <- spectra_import_server("spectra_import", stringsAsFactors = FALSE)

    # Import traits frame
    # traits_frame <- traits_import_server("traits_import", stringsAsFactors = FALSE)

    # Update info
    # validation_trait <- info_frame_server("frame_info", traits_frame)

    # Model arguments to past
    # models_arguments <- models_arguments_server("mod")

    ##############################################################################
    ### Functionality

    predicted_frame <- eventReactive(input$run, {

      # Predict traits on PLSR coefficients
      if(input$selection == "PLSR coefficients") {
        predicted_frame <- traits_predict(spectra_frame = spectra_frame(),
                                          coefficients = coefficients())

      # Predict traits using RTM
      } else {
        predicted_frame <- traits_predict(spectra_frame = spectra_frame(),
                                          coefficients = models_arguments()[[1]]$coefficients)
      }

      print(predicted_frame)

    })

    #Validate traits
    validation_plot_server("validation_figure",
                           observed = traits_frame,
                           predicted = predicted_frame,
                           arguments = models_arguments()[[1]]$arguments,
                           variable = validation_trait$observed)

    ##############################################################################
    ### Plot render modules

    #Return plot spectra
    callModule(spectra_plot_server,
               "spectra_figure",
               data = spectra_frame)

    #Return predicted plot
    callModule(predicted_plot_server,
               "predicted_figure",
               data = predicted_frame,
               arguments = models_arguments()[[1]]$arguments)

    ##############################################################################
    ### Table render modules

    #Validation input frame
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

    ##############################################################################
    ### Export modules

    #Export predicted traits
    callModule(traits_export_server,
               "traits_export",
               data = predicted_frame)
  })
}

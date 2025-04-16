################################################################################
### Build panel

build_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("SpecTraits"),
    p(""),
    h3("Build PLSR models"),
    br(""),
    fluidRow(

      #Upload panel
      column(3,
             p("You can build a PLSR model by uploading a .csv files that contains leaf reflectance spectra and leaf trait."),
             p("The spectra file most contain wavelengths (nm) as columns and samples as rows, a first column should be named ID."),
             p("The leaf trait file most contain traits as columns and samples as rows, a first column should be named ID."),
             p("We provide first a way to estimate the optimal number of components using two types of cross-validation."),
             p(""),
             HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
             br(""),

             wellPanel(
               h4("Step 1 - Import files"),
               spectra_import_ui(ns("spectra_import"), "Choose spectra file:"),
               traits_import_ui(ns("traits_import"), "Choose trait file:"),
               info_frame_ui(ns("frame_info"))
               ),

             wellPanel(
               h4("Step 2 - Data split approach"),
               split_input_ui(ns("method")),
               run_split_action_io(ns("run"),
               ),

             wellPanel(
               h4("Step 3 - Select the optimal number of components"),
               split_input_ui

               ),

             wellPanel(
               h4("Step 4 - Run iterative models"),
               # traits_import_ui(ns("traits_import"), "Choose file:"),
               # info_frame_ui(ns("frame_info")),
               tags$hr()),

             wellPanel(
               h4("Step 5 - Export models"),
               # traits_export_ui(ns("traits_export"), "Download predicted traits"),
               tags$hr())

      ),

      #Out and visualization panel
      column(9,
             tabsetPanel(type = "tabs",

                         #Plot spectra
                         tabPanel("Plot files",
                                  # spectra_plot_ui(ns("spectra_figure"))
                                  ),

                         #Plot predicted leaf traits
                         tabPanel("Data split approach",
                                  # predicted_plot_ui(ns("predicted_figure"))
                                  ),

                         #Validation file
                         tabPanel("Optimal number of components",
                                  # DT::dataTableOutput(ns("traits_df"))
                                  ),

                         #Validate prediction
                         tabPanel("PLSR models",
                                  # validation_plot_ui(ns("validation_figure"))
                                  )

                         #Summary report for predicted leaf traits
                         #tabPanel("Summary",
                         #          DT::dataTableOutput("coeff_df"))
             )
      )
    )
  )
}

# Analysis panel module Server logic
build_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Import files (Step 1) ----------------------------------------------------

    # Import file of spectra
    spectra_import <- spectra_import_server("spectra_import")

    # Import file of traits
    traits_import <- traits_import_server("traits_import")

    # Validation input frame
    output$traits_df <- DT::renderDataTable(DT::datatable(
      traits_import(),
      options = list(rowCallback = DT::JS(
        'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
      ))
    ))

    # Select trait for model
    trait_selection <- info_frame_server("frame_info", traits_import)


    # Data split (Step 1) ------------------------------------------------------

    # group
    # distribution
    # random
    # none




  })
}

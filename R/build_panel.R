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
               trait_selector_ui(ns("trait_selector"))
               ),

             wellPanel(
               h4("Step 2 - Define data split approach"),
               split_input_ui(ns("split_method")),
               # run_split_action_io(ns("run")),
               ),

             wellPanel(
               h4("Step 3 - Select the optimal number of components"),
               # press_input_ui(ns("optimal"))
               ),

             wellPanel(
               h4("Step 4 - Run PLSR models"),
               # traits_import_ui(ns("traits_import"), "Choose file:"),
               # info_frame_ui(ns("frame_info")),
             ),

             wellPanel(
               h4("Step 5 - Export models"),
               # traits_export_ui(ns("traits_export"), "Download predicted traits"),
             )

      ),

      #Out and visualization panel
      column(9,
             tabsetPanel(type = "tabs",

                         #Plot spectra
                         tabPanel("Plot files",
                                  build_import_plot_ui(ns("build_import_plot"))
                                  ),

                         #Plot predicted leaf traits
                         tabPanel("Data split",
                                  # predicted_plot_ui(ns("predicted_figure"))
                                  ),

                         #Validation file
                         tabPanel("PRESS",
                                  # DT::dataTableOutput(ns("traits_df"))
                                  ),

                         #Validate prediction
                         tabPanel("PLSR models",
                                  # validation_plot_ui(ns("validation_figure"))
                                  )

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
    reactive({print(spectra_import)})

    # Import file of traits
    traits_import <- traits_import_server("traits_import")

    # Select trait for model
    trait_selector <- trait_selector_sever("trait_selector", traits_import)

    # Plot observation
    build_import_plot_server("build_import_plot",
                             spectra = spectra_import,
                             trait = traits_import,
                             variable = trait_selector)


    # Data split (Step 2) ------------------------------------------------------

    split_method <- split_input_server("split_method", traits_import)

    # Optional: reactively trigger side effects #### THIS CAN BE REMOVE
    observeEvent(split_method(), {
      cat("[INFO] Method selected:",
          split_method()$split, "\n",
          split_method()$ratio, "\n",
          split_method()$gruop)
      }
    )

    # group
    # distribution
    # random
    # none

  })
}

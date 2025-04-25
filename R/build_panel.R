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
               run_split_action_ui(ns("run_split")),
               ),

             wellPanel(
               h4("Step 3 - Define optimal number of components"),
               press_input_ui(ns("press_method")),
               run_press_action_ui(ns("run_press"))
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
                                  split_action_plot_ui(ns("split_figure"))
                                  ),

                         #Validation file
                         tabPanel("PRESS",
                                  press_action_plot_ui(ns("press_figure"))
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
    # reactive({print(spectra_import)})

    # Import file of traits
    traits_import <- traits_import_server("traits_import")

    # observeEvent(traits_import(), {
    #   print(head(traits_import()))
    # })

    # Select trait for model
    trait_selector <- trait_selector_sever("trait_selector", traits_import)

    # observeEvent(trait_selector(), {
    #   cat(trait_selector())
    # })

    # Plot observation
    build_import_plot_server("build_import_plot",
                             spectra = spectra_import,
                             trait = traits_import,
                             variable = trait_selector)


    # Data split (Step 2) ------------------------------------------------------

    # Select split method
    split_method <- split_input_server("split_method", traits_import)

    # Apply method after definition
    split_vector <- run_split_action_server("run_split",
                                            trait_frame = traits_import(),
                                            trait_selector = trait_selector(),
                                            method = split_method()$split,
                                            ratio = split_method()$ratio,
                                            group = split_method()$group)

    # Plot data split
    split_action_plot_server("split_figure",
                             spectra = spectra_import,
                             trait = traits_import,
                             trait_selector = trait_selector,
                             split_vector = split_vector,
                             group = split_method()$group)

    # Optimal number of components (Step 3) ------------------------------------

    # Select press method
    press_method <- press_input_server("press_method")

    # Apply press method after definition
    press_frame <- run_press_action_server("run_press",
                                           spectra_frame = spectra_import(),
                                           trait_frame = traits_import(),
                                           trait_selector = trait_selector(),
                                           split_vector = split_vector(),
                                           method = press_method()$method,
                                           maxcomp =  press_method()$maxcomp,
                                           prop = press_method()$permutation,
                                           iterations = press_method()$iterations)

    # Optional: reactively trigger side effects #### THIS CAN BE REMOVE
    observeEvent(press_frame(), {
      cat("[INFO] Method selected:", "\n")
      head(press_frame()$press)
    })

    # Plot press results
    press_action_plot_server("press_figure",
                             press_frame)


    # Data split (Step 2) ------------------------------------------------------


  })
}

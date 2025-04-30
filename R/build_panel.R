################################################################################
### Build panel

build_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<h3 style='color:#005F5F; font-weight:bold;'>Build Partial Least Squares Regression (PLSR) models</h3>"),
    br(" "),
    fluidRow(

      #Upload panel
      column(3,
             p("Build PLSR models by uploading .csv files that contains leaf spectra and leaf trait."),
             p("The spectra file most contain wavelengths (nm) as columns and samples as rows. The first column should be named ID."),
             p("The leaf trait file most contain traits as columns and samples as rows, The first column should also be named ID."),
             p("SpecTraits provide a way to estimate the optimal number of components using machine learning frameworks."),
             p("SpecTraits also provide a way to build and export PLSR models while assessing their performance."),
             p(""),
             p("An example of files containing leaf spectra and traits can be downloaded ",
               a("here", href = "https://github.com/ASCEND-BII/SpecTraits/tree/main/inst/extdata", target = "_blank")),
             br(""),

             wellPanel(
               h4("Step 1 - Import files"),
               spectra_import_ui(ns("spectra_import"), "Choose a spectra file:"),
               traits_import_ui(ns("traits_import"), "Choose a trait file:"),
               trait_selector_ui(ns("trait_selector"))
               ),
             br(),

             wellPanel(
               h4("Step 2 - Define data split approach"),
               split_input_ui(ns("split_method")),
               run_split_action_ui(ns("run_split")),
               ),
             br(),

             wellPanel(
               h4("Step 3 - Evaluate the optimal number of components"),
               press_input_ui(ns("press_method")),
               run_press_action_ui(ns("run_press"))
               ),
             br(),

             wellPanel(
               h4("Step 4 - Run final PLSR models"),
               final_optimal_input_ui(ns("optimal")),
               run_plsr_action_ui(ns("run_plsr_final")),
             ),
             br(),

             wellPanel(
               h4("Step 5 - Export models"),
               # traits_export_ui(ns("traits_export"), "Download predicted traits"),
             )

      ),

      #Out and visualization panel
      column(9,
             tabsetPanel(type = "tabs",

                         # Plot spectra and traits
                         tabPanel("Plot files",
                                  build_import_plot_ui(ns("build_import_plot"))
                                  ),

                         # Plot data split
                         tabPanel("Data split",
                                  split_action_plot_ui(ns("split_figure"))
                                  ),

                         # Plot optimal
                         tabPanel("Optimal",
                                  press_action_plot_ui(ns("press_figure"))
                                  ),

                         # Plot coefficients and VIP
                         tabPanel("Coefficients and VIP",
                                  coefficients_plot_ui(ns("coeff_figure"))
                                  ),

                         # Plot performance
                         tabPanel("Model performance",
                                  # performance_plot_ui(ns("performance_figure"))
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

    # Plot press results
    press_action_plot_server("press_figure",
                             press_frame)

    # Run final model (Step 4) -------------------------------------------------

    # Define final parameters
    final_method <- final_optimal_input_server("optimal")

    # observeEvent(final_method(), {
    #   print(trait_selector())
    # })

    # Run final models
    final_PLSR <- run_plsr_action_server("run_plsr_final",
                                          spectra_frame = spectra_import(),
                                          trait_frame = traits_import(),
                                          trait_selector = trait_selector(),
                                          split_vector = split_vector(),
                                          method = final_method()$method,
                                          ncomp =  final_method()$ncomp,
                                          prop = final_method()$permutation,
                                          iterations = final_method()$iterations)

    # Plot coefficients and vip
    coefficients_plot_server("coeff_figure",
                             results = final_PLSR,
                             method = final_method()$method)

    # Predict plot server
    results_predict <- build_plsr_predict_server("plsr_predict",
                                                 coefficients = final_PLSR()$coefficients,
                                                 spectra_frame = spectra_import(),
                                                 trait_frame = traits_import(),
                                                 trait_selector = trait_selector(),
                                                 split_vector = split_vector())

    # Plot performance
    performance_plot_server("performance_figure",
                            coefficients = final_PLSR()$coefficients,
                            spectra_frame = spectra_import(),
                            trait_frame = traits_import(),
                            trait_selector = trait_selector(),
                            split_vector = split_vector())

    # Export export model (Step 5) ----------------------------------------------

    # Export predicted traits
    # callModule(traits_export_server,
    #            "traits_export",
    #            data = predicted_frame)

  })
}

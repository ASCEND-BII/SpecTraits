################################################################################
#               ___________________________________________________            #
#                                   SpecTraits                                 #
#                               Spectra - Traits                               #
#                  A Shiny Application for prediction leaf traits              #
#                             from spectra models using R                      #
#                           Author: J. Antonio Guzmán Q.                       #
#               ___________________________________________________            #
#                                                                              #
################################################################################

# Name convention for scripts
# _input: all user information that serve as input
# _import: functions to read files
# _frame: all data.frames created
# _plot: scripts to render figures in ui
# _figure: all figures created as outputs
# _go: names to link between panels
# _action: name for bottom activation
# _panel: Major UI panels of visualization
# _aux: Auxiliary functions

################################################################################
# Libraries --------------------------------------------------------------------
################################################################################

# Define packages
packages <- c("shiny", "shinycssloaders",
              "bslib", "data.table", "dplyr",
              "pls", "DT", "here", "reshape2", "magrittr",
              "ggplot2", "rlang", "caret", "zip",
              "spectrolab")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Check rhdf5
if(!("ccrtm" %in% installed.packages())) {remotes::install_github("MarcoDVisser/ccrtm")}

# Required packages
sapply(c(packages, "ccrtm"), require, character.only = TRUE)

################################################################################
# Options ----------------------------------------------------------------------
################################################################################

# File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers ------------------------------------------------------------
################################################################################

# Home access functions
source("R/home_panel.R")
source("R/predict_panel.R")
source("R/build_panel.R")
source("R/preprocessing_panel.R")
source("R/about_panel.R")

# Functions for predict panel
source("R/predict/spectra_import.R")
source("R/predict/spectra_plot.R")
source("R/predict/method_input.R")
source("R/predict/run_action.R")
source("R/predict/plsr_traits_predict_aux.R")
source("R/predict/rtm_traits_predict_aux.R")
source("R/predict/predicted_plot.R")
source("R/predict/traits_import.R")
source("R/predict/validation_plot.R")
source("R/predict/traits_export.R")
source("R/predict/info_frame.R")

# Functions for build panel
source("R/build/trait_selector_input.R")
source("R/build/build_import_plot.R")
source("R/build/split_input.R")
source("R/build/run_split_action.R")
source("R/build/split_action_plot.R")
source("R/build/press_input.R")
source("R/build/run_press_action.R")
source("R/build/pls_permutation_press_aux.R")
source("R/build/press_action_plot.R")
source("R/build/find_optimal_ncomp_aux.R")
source("R/build/final_optimal_input.R")
source("R/build/run_plsr_action.R")
source("R/build/pls_permutation_coef_aux.R")
source("R/build/vip_aux.R")
source("R/build/pls_summary_aux.R")
source("R/build/confidence_interval_aux.R")
source("R/build/coefficients_plot.R")
source("R/build/build_plsr_predict.R")
source("R/build/plsr_predict_aux.R")
source("R/build/model_performance_aux.R")
source("R/build/performance_plot.R")
source("R/build/build_export.R")

# Functions for pre-processing
source("R/preprocessing/resample_input.R")
source("R/preprocessing/run_resample_action.R")
source("R/preprocessing/resampled_export.R")

################################################################################
# App---------------------------------------------------------------------------
################################################################################

# ------------------------------------------------------------------------------
# Define UI for application
ui <- page_navbar(
  title = "SpecTraits",
  id = "main_tabs",
  nav_spacer(),
  theme = bs_theme(bootswatch = "yeti",
                   primary = "#005F5F"),
  bg = "#005F5F",

  nav_panel(
    "Home",
    home_panel_ui("home")
  ),

  nav_panel(
    "Predict",
    predict_panel_ui("predict")
  ),

  nav_panel(
    "Build",
    build_panel_ui("build")
  ),

  nav_panel(
    "Resampling",
    preprocessing_panel_ui("preprocessing")
  ),

  nav_panel(
    "Data",
    # about_panel_ui("about")
  ),

  nav_panel(
    "About",
    about_panel_ui("about")
  ),

  nav_item(
    tags$a(icon("github"),
           "SourceCode",
           href = "https://github.com/ASCEND-BII/SpecTraits",
           target = "_blank")
  )
)

# ------------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  go_to_predict <- reactiveVal(FALSE)
  go_to_build <- reactiveVal(FALSE)
  go_to_prepro <- reactiveVal(FALSE)
  go_to_data <- reactiveVal(FALSE)

  home_panel_server("home", go_to_predict, go_to_build, go_to_prepro)
  predict_panel_server("predict")
  build_panel_server("build")
  preprocessing_panel_server("preprocessing")
  # data_panel_server("data")

  observeEvent(go_to_predict(), {
    if (go_to_predict()) {
      updateTabsetPanel(session, "main_tabs", selected = "Predict")
      go_to_predict(FALSE)
    }
  })

  observeEvent(go_to_build(), {
    if (go_to_build()) {
      updateTabsetPanel(session, "main_tabs", selected = "Build")
      go_to_build(FALSE)
    }
  })

  observeEvent(go_to_prepro(), {
    if(go_to_prepro()) {
      updateTabsetPanel(session, "main_tabs", selected = "Pre-process")
      go_to_prepro(FALSE)
    }
  })

  observeEvent(go_to_data(), {
    if(go_to_prepro()) {
      updateTabsetPanel(session, "main_tabs", selected = "Data")
      go_to_data(FALSE)
    }
  })
}

# ------------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)

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

library(shiny)
library(shinycssloaders)
library(bslib)
library(data.table)
library(dplyr)
library(pls)

library(DT)
library(here)
library(reshape2)
library(magrittr)
library(ggplot2)
library(rlang)
library(caret)
library(metrica)

if(!require(prospect)){
  remotes::install_github('jbferet/prospect')
  library(prospect)
} else {
  library(prospect)
}

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
source("home_panel.R")
source("predict_panel.R")
source("build_panel.R")
source("preprocessing_panel.R")
source("about_panel.R")

# Functions for predict panel
source("spectra_import.R")
source("spectra_plot.R")
source("method_input.R")
source("run_action.R")
source("plsr_traits_predict_aux.R")
source("rtm_traits_predict_aux.R")
source("predicted_plot.R")
source("traits_import.R")
source("validation_plot.R")
source("traits_export.R")
source("info_frame.R")

# Functions for build panel
source("trait_selector_input.R")
source("build_import_plot.R")
source("split_input.R")
source("run_split_action.R")
source("split_action_plot.R")
source("press_input.R")
source("run_press_action.R")
source("pls_permutation_press_aux.R")
source("press_action_plot.R")
source("find_optimal_ncomp_aux.R")
source("final_optimal_input.R")
source("run_plsr_action.R")
source("pls_permutation_coef_aux.R")
source("vip_aux.R")
source("pls_summary_aux.R")
source("confidence_interval_aux.R")
source("coefficients_plot.R")
source("build_plsr_predict.R")
source("plsr_predict_aux.R")
source("model_performance_aux.R")
source("performance_plot.R")
source("build_export.R")

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
    "Pre-process",
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

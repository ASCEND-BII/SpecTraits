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

# Name convention for coding
# _input: all ASCII files that serve as input
# _import: functions to read files
# _frame: all data.frames created
# _plot: functions to create figures
# _figure: all figures created as outputs
# _go: names to link between panels
# _action: name for bottom activation

################################################################################
#Libraries----------------------------------------------------------------------
################################################################################

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(data.table)
library(dplyr)

library(DT)
library(here)
library(reshape2)
library(magrittr)
library(ggplot2)
library(rlang)
library(metrica)

if(!require(prospect)){
  remotes::install_github('jbferet/prospect')
  library(prospect)
} else {
  library(prospect)
}

################################################################################
#Options------------------------------------------------------------------------
################################################################################

# File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
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
source("plsr_traits_predict.R")
source("rtm_traits_predict.R")
source("predicted_plot.R")
source("traits_import.R")
source("validation_plot.R")
source("traits_export.R")
source("info_frame.R")

# Functions for build panel

################################################################################
#App----------------------------------------------------------------------------
################################################################################

# Define UI for application
ui <- function(){

  navbarPage("SpecTraits",
             theme = shinythemes::shinytheme("cerulean"),
             tabsetPanel(id = "main_tabs",
                         tabPanel("Home", home_panel_ui("home")),
                         tabPanel("Predict", predict_panel_ui("predict")),
                         tabPanel("Build", build_panel_ui("build")),
                         tabPanel("Pre-processing", preprocessing_panel_ui("preprocessing")),
                         tabPanel("About", about_panel_ui("about"))
             ))


}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # utils::globalVariables(c(".", ".N", ".SD", ".I", ".GRP", ".BY", ".."))

  go_to_predict <- reactiveVal(FALSE)
  go_to_build <- reactiveVal(FALSE)
  go_to_prepro <- reactiveVal(FALSE)

  home_panel_server("home", go_to_predict, go_to_build, go_to_prepro)
  predict_panel_server("predict")
  build_panel_server("build")
  preprocessing_panel_server("preprocessing")

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
      updateTabsetPanel(session, "main_tabs", selected = "Pre-processing")
      go_to_prepro(FALSE)
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

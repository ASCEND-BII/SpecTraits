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
library(dplyr)
library(DT)
library(here)
library(reshape2)
library(magrittr)
library(ggplot2)
library(shinythemes)
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
#File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
################################################################################

# Home access functions
source("home_panel.R")
source("predict_panel.R")
source("build_panel.R")
source("about_panel.R")

# Import functions
source("spectra_import.R")
source("traits_import.R")
source("info_frame.R")

# Figure
source("spectra_plot.R")
source("predicted_plot.R")
source("validation_plot.R")

# Functionality on predict models
source("models.R")
source("match_range.R")
source("traits_predict.R")

# Export functions
source("traits_export.R")

################################################################################
#App----------------------------------------------------------------------------
################################################################################

# Define UI for application that draws a histogram
ui <- function(){

  bootstrapPage('',

                tags$style(type = 'text/css',
                           HTML('.navbar {background-color: #0097a7ff; font-size: 18px;}
                           .navbar-default .navbar-brand {color: #ffffff; font-size: 20px;}
                           .navbar-default .navbar-nav > .active > a,
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover {color: #ffffff; background-color: #659ca2ff;}
                           .well {background-color: #dcf0f2ff;}' #Background boxes
                           )
                ),

                navbarPage("SpecTraits",
                           tabPanel("Home", home_panel_ui("home")),
                           tabPanel("Predict", predict_panel_ui("predict")),
                           tabPanel("Build", build_panel_ui("build")),
                           tabPanel("About", about_panel_ui("about"))
                           )
                )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Function to update the active tab
  updateTab <- function(panel) {
    updateTabsetPanel(session, "tabs", selected = panel)
  }


  # Panels with actions
  home_panel_server("home", updateTab)
  predict_panel_server("predict")
  build_panel_server("build")

}

# Run the application
shinyApp(ui = ui, server = server)

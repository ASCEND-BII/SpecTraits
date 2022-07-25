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
library(plotly)
library(shinythemes)
library(rlang)
library(metrica)

################################################################################
#Options------------------------------------------------------------------------
################################################################################
#File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
################################################################################

#import functions
source("spectra_import.R")
source("traits_import.R")
source("info_frame.R")

#figure
source("spectra_plot.R")
source("predicted_plot.R")
source("validation_plot.R")

#functionality
source("models.R")
source("match_range.R")
source("traits_predict.R")

#export functions
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

                           #####################################################
                           ###---Home panel panel-------------------------------
                           #####################################################

                           tabPanel("Home",
                                    fluidRow(column(width = 6,
                                                    offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>SpecTraits 0.1 </h1>"),
                                                              h4(),
                                                              HTML("<p>SpecTraits is a shiny application aiming at predicting and building models to infer about leaf traits using reflectance spectra"),
                                                              p("SpecTraits has two main functionalities:
                                                                  i) predicting leaf traits from publised PLSR coefficients or radiative transfer models, and
                                                                  ii) build PLSR models from user-defined data set of leaf traits-spectra.
                                                                "),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              fluidRow(actionButton(inputId = "predict",
                                                                                    label = 'Access the application'),
                                                                       align = "center"),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              img(src = "plsr.png", width = "400px", height = "400px", align = "center"),
                                                              p(""),
                                                              p(""))
                                                    )
                                             )
                           ),

                           #####################################################
                           ###---Application panel------------------------------
                           #####################################################

                           navbarMenu("Application",

                                      ####################################################
                                      ###---Predict panel---###
                                      tabPanel(title = "Predict leaf traits using leaf spectra",
                                               id = "predict",
                                               value = "goal1",
                                               fluidRow(

                                                 #Main panel
                                                 column(12,
                                                        h2("SpecTraits"),
                                                        p(""),
                                                        h3("Predict leaf traits using leaf spectra"),
                                                        br(""),
                                                        fluidRow(

                                                          #Upload panel
                                                          column(3,
                                                                 p("You can predict leaf traits by by uploading an ASCII file that contains leaf spectra."),
                                                                 p("The spectra file most contain wavelengths as columns and samples as rows, a first column should be named ID."),
                                                                 HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                                                 br(""),

                                                                 wellPanel(
                                                                   h4("Import files"),
                                                                   spectra_import_ui("spectra_import", "Choose spectra:")),

                                                                 wellPanel(
                                                                   h4("Selection of arguments"),
                                                                   models_IU("mod")),

                                                                 wellPanel(
                                                                   h4("External validation (optional)"),
                                                                   traits_import_ui("traits_import", "Choose file:"),
                                                                   info_frame_ui("frame_info"),
                                                                   tags$hr()),

                                                                 wellPanel(
                                                                   h4("Export predicted traits"),
                                                                   traits_export_ui("traits_export", "Download predicted traits"),
                                                                   tags$hr())
                                                          ),

                                                          #Out and visualization panel
                                                          column(9,
                                                                 tabsetPanel(type = "tabs",

                                                                             #Plot spectra
                                                                             tabPanel("Plot spectra",
                                                                                      spectra_plot_ui("spectra_figure")),

                                                                             #Plot predicted leaf traits
                                                                             tabPanel("Predicted leaf trait",
                                                                                      predicted_plot_ui("predicted_figure")),

                                                                             #Validation file
                                                                             tabPanel("External validation file",
                                                                                      DT::dataTableOutput("traits_df")),

                                                                             #Validate prediction
                                                                             tabPanel("Leaf trait validation",
                                                                                      validation_plot_ui("validation_figure")),

                                                                             #Summary report for predicted leaf traits
                                                                             tabPanel("Summary",
                                                                                      DT::dataTableOutput("coeff_df"))
                                                                 )
                                                          )
                                                        )
                                                 )
                                               )
                                      ),

                                      ####################################################
                                      ###---Build panel---###
                                      tabPanel("Build your own model")),

                           #####################################################
                           ###---About Panel------------------------------------
                           #####################################################

                           tabPanel("About",
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>SpecTraits 0.1 </h1>"),
                                                              p("2022-07-16", align = "center"),
                                                              HTML("<p align = 'center'><img src = 'github.png' width = '20px' height = 'auto'> <a target='_blank' rel='noopener noreferrer' href='https://github.com/Antguz/SpecTraits'> We are on GitHub </a></p>"),
                                                              HTML("<p><b>Cite the application:</b> https://doi.org/ '>https://doi.org/10.1002/ece3.6928</a></p>")
                                                    )
                                    )
                                    )
                           )
                )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ##############################################################################
  ###Images---------------------------------------------------------------------
  ##############################################################################



  ##############################################################################
  ###Frames---------------------------------------------------------------------
  ##############################################################################

  #Spectra to import
  spectra_frame <- spectra_import_server("spectra_import", stringsAsFactors = FALSE)

  #Import traits frame
  traits_frame <- traits_import_server("traits_import", stringsAsFactors = FALSE)

  #Update info
  validation_trait <- info_frame_server("frame_info", traits_frame)

  #Model arguments to past
  models_arguments <- models_arguments_server("mod")

  ##############################################################################
  ###Functionality--------------------------------------------------------------
  ##############################################################################

  #Predict trait
  predicted_frame <- reactive({

    traits_predict(spectra_frame = spectra_frame(),
                   coefficients = models_arguments()[[1]]$coefficients,
                   model = models_arguments()[[1]]$arguments[7])
  })

  #Validate traits
  validation_plot_server("validation_figure",
                         observed = traits_frame,
                         predicted = predicted_frame,
                         arguments = models_arguments()[[1]]$arguments[7],
                         variable = validation_trait$observed)

  ##############################################################################
  ###Plot render modules--------------------------------------------------------
  ##############################################################################

  #Return plot spectra
  callModule(spectra_plot_server,
             "spectra_figure",
             data = spectra_frame)

  #Return predicted plot
  callModule(predicted_plot_server,
             "predicted_figure",
             data = predicted_frame,
             arguments = models_arguments()[[1]]$arguments[7])

  ##############################################################################
  ###Table render modules-------------------------------------------------------
  ##############################################################################

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

  #Validation input frame
  output$coeff_df <- DT::renderDataTable(DT::datatable(
    options = list(rowCallback = DT::JS(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
    ))
  ))

  ##############################################################################
  ###Export modules-------------------------------------------------------------
  ##############################################################################

  #Export predicted traits
  callModule(traits_export_server,
             "traits_export",
             data = predicted_frame)

}

# Run the application
shinyApp(ui = ui, server = server)

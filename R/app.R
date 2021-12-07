################################################################################
#               ___________________________________________________            #
#                                   SpecTraits                                 #
#                               Spectra - Traits                               #
#               A Shiny Application for prediction leaf traits                 #
#                             from spectra models using R                      #
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

################################################################################
#Options------------------------------------------------------------------------
################################################################################
#File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
################################################################################

#import function
source("spectra_import.R")
source("traits_import.R")

#figure
source("spectra_plot.R")
source("predicted_plot.R")

#functionality
source("plsr_models.R")
source("match_range.R")
source("predict_traits.R")

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
                                                              HTML("<p>SpecTraits is a shiny application aiming at predicting and building PLSR models to infer about leaf traits using leaf spectra"),
                                                              p("SpecTraits has two main functionalities:
                                                                  i) predicting leaf traits from publised PLSR coefficients, and
                                                                  ii) build PLSR models from user-defined data set of leaf traits-spectra.
                                                                "),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              fluidRow(actionButton(inputId = "predit_go",
                                                                                    label = 'Access the application'),
                                                                       align = "center"),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              img(src = "../inst/app/www/plsr.png", width = "400px", height = "400px"),
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
                                                                   h4("Model selection"),
                                                                   plsr_models_IU("mod", "Model:")),

                                                                 wellPanel(
                                                                   h4("External validation (optional)"),
                                                                   traits_import_ui("traits_import", "Choose file:"),
                                                                   tags$hr(),
                                                                   selectInput("traits_colnames", "Select trait", ""))
                                                          ),

                                                          #Out and visualization panel
                                                          column(9,
                                                                 tabsetPanel(type = "tabs",

                                                                             #Leaf spectra
                                                                             tabPanel("Leaf spectra file",
                                                                                      DT::dataTableOutput("spectra_df")),

                                                                             #Validation file
                                                                             tabPanel("External validation file",
                                                                                      DT::dataTableOutput("traits_df")),

                                                                             #Plot spectra
                                                                             tabPanel("Plot spectra",
                                                                                      spectra_plot_ui("spectra_figure")),

                                                                             #Plot predicted leaf traits
                                                                             tabPanel("Predicted leaf trait"),

                                                                             #Validate prediction
                                                                             tabPanel("Leaf trait validation"),

                                                                             #Summary report for predicted leaf traits
                                                                             tabPanel("Summary",
                                                                                      dataTableOutput("coeff"))
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
                                                              p("2021-11-19", align = "center"),
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
  ###Frames---------------------------------------------------------------------
  ##############################################################################

  #Spectra to import
  spectra_frame <- spectra_import_server("spectra_import", stringsAsFactors = FALSE)

  #Import traits frame
  traits_frame <- traits_import_server("traits_import", stringsAsFactors = FALSE)

  #Published coefficients
  plsr_coefficients  <- plsr_models_server("mod")

  ##############################################################################
  ###Functionality--------------------------------------------------------------
  ##############################################################################

  ##############################################################################
  ###Render modules-------------------------------------------------------------
  ##############################################################################

  #Spectra output frame
  output$spectra_df <- DT::renderDataTable(DT::datatable(
    spectra_frame(),
    options = list(rowCallback = DT::JS(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
    ))
  ))


  #Return plot spectra
  callModule(spectra_plot_server,
             "spectra_figure",
             data = spectra_frame)


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

  #Coefficients frame
  output$coeff <- DT::renderDataTable(DT::datatable(
    plsr_coefficients(),
    options = list(rowCallback = DT::JS(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
    ))
  ))
}

# Run the application
shinyApp(ui = ui, server = server)

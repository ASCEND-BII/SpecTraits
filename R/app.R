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
library(data.table)
library(reshape2)
library(magrittr)
library(ggplot2)
library(shinythemes)

################################################################################
#Options------------------------------------------------------------------------
################################################################################
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
################################################################################

#import function
source("frame_import.R")


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

                           ############################################################################
                           ###---Home panel panel------------------------------------------------------
                           ############################################################################

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
                                                              img(src = "inst/app/www/plsr.png", width = '100%', height = "auto"),
                                                              p(""),
                                                              p(""))
                                    )
                                    )
                           ),

                           ##########################################################################
                           ###---Application panel---------------------------------------------------
                           ##########################################################################

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
                                                                   import_UI("spectra_import", "Choose spectra to import"),
                                                                   import_UI("traits_import", "Choose traits to import and validate (optional)"),
                                                                   actionButton("plot_spectra_action", "Plot spectra"),
                                                                 ),
                                                                 h4("Model selection"),
                                                                 wellPanel(
                                                                   fluidRow(
                                                                     column(width = 9,
                                                                            selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)" = "to_send.rda",
                                                                                                                       "LMA (Serbin et al. 2019)" = "Serbin_2019"))
                                                                     )
                                                                   ),
                                                                   actionButton("predict_action", "Predict trait"),
                                                                 )
                                                          ),

                                                          #Out and visualization panel
                                                          column(9,
                                                                 tabsetPanel(type = "tabs",

                                                                             #Plot spectra
                                                                             tabPanel("Plot spectra",
                                                                                      plotOutput("spectra_figure", height = 700)),

                                                                             #Plot predicted leaf traits
                                                                             tabPanel("Predicted leaf trait",
                                                                                      plotOutput("predicted_figure", height = 700)),

                                                                             #Summary report for predicted leaf traits
                                                                             tabPanel("Summary", dataTableOutput("spectra_table"))
                                                                 )
                                                          )
                                                        )
                                                 )
                                               )
                                      ),

                                      ####################################################
                                      ###---Build panel---###
                                      tabPanel("Build your own model")),

                           ##########################################################################
                           ###---About Panel---###
                           ##########################################################################

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

  #Spectra to import
  spectra_frame <- import_server("spectra_import", stringsAsFactors = FALSE)

  #Leaf traits to import
  traits_frame <- import_server("traits_import", stringsAsFactors = FALSE)

  output$spectra_table <- renderDataTable({
    spectra_frame()
  })

  output$traits_table <- renderDataTable({
    traits_frame()
  })

}

# Run the application
shinyApp(ui = ui, server = server)

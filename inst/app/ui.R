################################################################################
#               ___________________________________________________            #
#                                   SpecTraits                                 #
#                               Spectra - Traits                               #
#               A Shiny Application for prediction leaf traits                 #
#                             from spectra models using R                      #
#               ___________________________________________________            #
#                                                                              #
################################################################################

# Name convention for the coding
# _input: all ASCII that serve as input
# _load: functions to read files
# _frame: all ASCII file after reading
# _figure: all functions to create plots
# _plot: all figures that serve as outputs

################################################################################
#Libraries----------------------------------------------------------------------
################################################################################

library(shiny)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(shinythemes)

################################################################################
#App----------------------------------------------------------------------------
################################################################################

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
                                                              fluidRow(actionButton('app', 'Access the application'), align = "center"),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              img(src = 'plsr.png', width = '100%', height = "auto"),
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
                                      tabPanel("Predict leaf traits using leaf spectra",
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
                                                                 h4("Load spectra file"),
                                                                 wellPanel(
                                                                   fileInput('spectra_input', 'Choose spectra file',
                                                                             accept=c('text/csv', 
                                                                                      'text/comma-separated-values,text/plain', 
                                                                                      '.csv')
                                                                   ),
                                                                   actionButton("spectra_plot", "Plot spectra"),
                                                                 ),
                                                                 h4("Model selection"),
                                                                 wellPanel(
                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)",
                                                                                                                       "LMA (Serbin et al. 2019)"))
                                                                     )
                                                                   ),
                                                                   actionButton("trait_predict", "Predict trait"),
                                                                 )
                                                            ),
                                                          
                                                          #Out and visualization panel
                                                          column(9,
                                                                 fluidRow(
                                                                   
                                                                   #Plot spectra and 
                                                                   column(5,
                                                                          h4("Plot spectra"),
                                                                          plotOutput('spectra_plot', height = '563px')),
                                                                   
                                                                   #Plot processed spectra and predicted values
                                                                   column(4,
                                                                          h4("Coefficient of variation"))
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
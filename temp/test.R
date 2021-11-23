spectra_frame <- read.csv("inst/extdata/spectra.csv", check.names = FALSE)
wavelength <- as.numeric(colnames(spectra_frame)[-1])


frame_melt <- spectra_frame %>% melt(id.vars = "ID", 
                                     variable = "Wavelength",
                                     value.name = "Reflectance")



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
library(shiny)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(shinythemes)

################################################################################
#App----------------------------------------------------------------------------
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
                
                ###---Home panel panel---###
                navbarPage(title = "SpecTraits", id = "tabset", 
                           tabPanel("Home page",
                                    fluidRow(column(width = 6, offset = 3,
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
                                                              p(""),
                                                    )
                                            )
                                    )
                           ),
                           
                           ###---Application panel---###
                           tabPanel("Application", value = "app",
                                    fluidPage(
                                      h1("SpecTraits"),
                                      h4(""),
                                      fluidRow(
                                        column(width = 4,
                                               tabsetPanel(
                                                 
                                                 ###---Predict panel---###
                                                 tabPanel("Predict leaf traits using leaf spectra",
                                                          p(""),
                                                          h3("Predict leaf traits using leaf spectra"),
                                                          br(""),
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
                                                              column(width = 6,
                                                                     selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)",
                                                                                                                "LMA (Serbin et al. 2019)"))
                                                              )
                                                            ),
                                                            actionButton("trait_predict", "Predict trait"),
                                                          )
                                                 ),
                                                 
                                                 ###---Build panel---###
                                                 tabPanel("Build your own model",
                                                          p(""),
                                                          h3("Build a PLSR model to predict leaf traits"),
                                                          br(""),
                                                          p("You can create your own model by uploading and two ASCII files that contains leaf spectra and leaf traits."),
                                                          p("The spectra file most contain wavelengths as columns and samplesa as rows, a first column should be named ID."),
                                                          p("The trait file most contain traits as columns and samples as rows, a first column should be also named ID."),
                                                          p("The ID columns in both spectra and trait file is the join element between databases to build the model."),
                                                          HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                                          br(""),
                                                          h4("Load spectra file"),
                                                          wellPanel(
                                                            fileInput('spectra_input', 'Choose spectra file',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv'))
                                                          ),
                                                          h4("Load leaf trait file"),
                                                          wellPanel(
                                                            fileInput('trait_frame', 'Choose trait file',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv'))
                                                          )
                                                 )
                                               )
                                        ),
                                        fluidRow(
                                          column(12,
                                                 fluidRow(
                                                   column(6,
                                                          plotOutput('spectra_plot', height = '563px')
                                                   ),
                                                   column(6,
                                                          p("dd")
                                                   )
                                                 )
                                          ),
                                        )
                                      )
                                   )
                                    
                           )
                )
  )
}



fluidRow(
  column(12,
         fluidRow(
           column(6,
                  plotOutput('spectra_plot', height = '563px')
           ),
           column(6,
                  p("dd")
           )
         )
  )
)












tabPanel("Predict leaf traits using leaf spectra",
         fluidRow(
           column(3,
                  h2("SpecTraits"),
                  br(),
                  h3("Predict leaf traits using leaf spectra"),
                  br(),
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
                  h4("Select model"),
                  wellPanel(
                    fluidRow(
                      column(width = 6,
                             selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)",
                                                                        "LMA (Serbin et al. 2019)"))
                      )
                    ),
                    actionButton("trait_predict", "Predict trait"),
                  )
           )
         )
)
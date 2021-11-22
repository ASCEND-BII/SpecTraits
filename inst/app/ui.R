#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Libraries----------------------------------------------------------------------
library(shiny)
library(data.table)
library(ggplot2)

#Options------------------------------------------------------------------------
options(shiny.deprecation.messages=FALSE)

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
                navbarPage(title = "SpecTrait", id = "tabset", 
                           tabPanel("Home page",
                                    
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>SpecTrait 0.1 </h1>"),
                                                              br(),
                                                              HTML("<p>SpecTrait is a shiny application aiming at predicting and building PLSR models to predict leaf traits
                                                                    using leaf spectra"),
                                                              p("SpecTrait has two main functionalities: 
                                                                  i) predicting leaf traits from publised PLSR coefficients, and 
                                                                  ii) build PLSR models from user-defined data set of leaf traits-spectra.
                                                                "),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              fluidRow(actionButton("app", "Access the application"), align = "center"),
                                                              p(""),
                                                              p(""),
                                                              p(""),
                                                              img(src = "plsr.png", width = '100%', height = "auto"),
                                                              p(""),
                                                              p(""),
                                                    )
                                            )
                                    )
                           ),
                           tabPanel("Application", value = "app",
                                    fluidPage(
                                      p("Visualizing and predicting leaf traits using leaf spectra"),
                                      fluidRow(
                                        column(width = 4,
                                               tabsetPanel(
                                                 tabPanel("Predict leaf traits using leaf spectra",
                                                          p(""),
                                                          h4("Predict leaf traits using leaf spectra"),
                                                          p("You can predict leaf traits by by uploading an ASCII file that contains leaf spectra."),
                                                          p("The spectra file most contain wavelengths as columns and rows as samples, a first column should be named ID."),
                                                          HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                                          wellPanel(
                                                            fileInput('spectra', 'Choose spectra file',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv'))
                                                          ),
                                                          wellPanel(
                                                            fluidRow(
                                                              h4("Select model:"),
                                                              column(width = 6,
                                                                     selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)",
                                                                                                                "LMA (Serbin et al. 2019)"))
                                                              )
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Build your own model",
                                                          p(""),
                                                          h4("Build a PLSR model to predict leaf traits"),
                                                          p("You can create your own model by uploading and two ASCII files that contains leaf spectra and leaf traits."),
                                                          p("The spectra file most contain wavelengths as columns and rows as samples, a first column should be named ID."),
                                                          p("The trait file most contain traits as columns and samples as rows, a first column should be also named ID."),
                                                          p("The ID columns in both spectra and trait file is the join element between databases to build the model."),
                                                          HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                                          wellPanel(
                                                            fileInput('trait', 'Choose trait file',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv'))
                                                          ),
                                                          wellPanel(
                                                            fileInput('spectra', 'Choose spectra file',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv'))
                                                          )
                                                 ),
                                               )
                                        )
                                      )    
                                    )
                           )
                )
  )
}
                                         




#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  tabPanel("Shiny application", value = "app",
           fluidPage(
             p("Visualizing and predicting leaf traits using leaf spectra"),
             fluidRow(
               column(width = 4,
                      tabsetPanel(
                        tabPanel("Predict leaf traits using leaf spectra",
                                 p(""),
                                 h4("Selection of traits, growth forms and components"),
                                 HTML("<p>Dynamic visualization of the principal components
                                                with a customized selection of traits and growth forms using 
                                                the original data set analyzed in Díaz <i>et al.</i> (2016)</p>"),
                                 wellPanel(
                                   fluidRow(
                                     column(width = 6,
                                            h4("Traits:"),
                                            checkboxInput('LA', 'Leaf area (LA)', TRUE),
                                            checkboxInput('Nmass', 'Leaf nitrogen content per unit mass (Nmass)', TRUE),
                                            checkboxInput('LMA', 'Leaf mass per area (LMA)', TRUE),
                                            checkboxInput('H', 'Plant height (H)', TRUE),
                                            checkboxInput('SM', 'Diaspore mass (SM)', TRUE),
                                            checkboxInput('SSD', 'Stem specific density (SSD)', TRUE)
                                     ),
                                     column(width=6,
                                            h4("Growth Forms:"),
                                            checkboxInput('herb', 'Herbs (n=1166)', TRUE),
                                            checkboxInput('shrub', 'Shrubs (n=173)', TRUE),
                                            checkboxInput('tree', 'Trees (n=846)', TRUE),
                                            checkboxInput('other', 'Others (n=29)', TRUE)
                                     )),
                                   fluidRow(
                                     h4("Components:"),
                                     column(width = 6,
                                            selectInput("axis1", "Axis 1:", choices = c("PC1","PC2", "PC3", "PC4", "PC5"))
                                     ),
                                     column(width = 6,
                                            selectInput("axis2", "Axis 2:", choices = c("PC2","PC3", "PC4", "PC5", "PC6"))
                                     )
                                   )
                                 ),
                                 h4("Density areas and species identification"),
                                 p("Customize density areas according to several criteria and
                                                            indentify a set of species within the customized PCA"),
                                 wellPanel(
                                   fluidRow(
                                     h4("Colored area"),
                                     selectInput("theme", "", choices = c("All species", "Growth forms", "Herbs only", "Shrubs only", "Trees only")),
                                     h4("Find a species point:"),
                                     column(textInput("find", "Type the first letters:", ""), width=6),
                                     conditionalPanel(
                                       condition = "input.find != ''",
                                       column(selectInput("species", "Select a species name:", choices=c("NA")), width=6)
                                     ),
                                     conditionalPanel(
                                       condition = "output.sp != ''",
                                       column(actionButton("reset", "Clear all"), width = 12, align = "right")
                                     )
                                   ))
                        ),
                        tabPanel("Build your own model",
                                 p(""),
                                 p("You can create your own model by uploading and two ASCII files that contains leaf spectra and leaf traits."),
                                 p("The spectra file most contain wavelengths as columns and rows as samples, a first column should be named ID."),
                                 p("The trait file most contain traits as columns and samples as rows, a first column should be also named ID."),
                                 p("The ID column in both spectra and trait file is the join element between databases."),
                                 HTML("<p> An example of files containing leaf traits and spectra can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                 wellPanel(
                                   fileInput('file1', 'Choose ASCII File',
                                             accept=c('text/csv', 
                                                      'text/comma-separated-values,text/plain', 
                                                      '.csv')),
                                   htmlOutput("upload"),
                                   hr(),
                                   fluidRow(column(radioButtons('sep', 'Separator',
                                                                c(Comma=',',
                                                                  Semicolon=';',
                                                                  Tab='\t'),
                                                                ','),width = 6),
                                            column(radioButtons("dec", "Decimal",
                                                                c(Dot='.',
                                                                  Comma=','),
                                                                "."),width = 6)),
                                  )
                        ),
                        )),
               column(width = 7,
                      conditionalPanel(
                        condition = "!output.meme",
                        plotOutput('PCAPlot',height = 700),
                        htmlOutput("sp")
                      ),
                      conditionalPanel(
                        condition = "output.meme",
                        h1("Error, please reconsider your panel selections", align = "center", style="margin:150px 0px 10px 0px"),
                        img(src = "meme.jpg", width = '300px', height = "auto"),
                        align = "center"
                      )
               )
             )    
           )
  )
))

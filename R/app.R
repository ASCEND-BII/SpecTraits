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
# _go: names to link between panels
# _action: name for bottom activation

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
                                                                 h4("Load spectra file"),
                                                                 wellPanel(

                                                                   fileInput('spectra_input', 'Choose file to upload',
                                                                             accept = c(
                                                                               'text/csv',
                                                                               'text/comma-separated-values',
                                                                               '.csv'
                                                                             )),
                                                                   htmlOutput("upload"),
                                                                   hr(""),
                                                                   fluidRow(column(radioButtons('sep', 'Separator',
                                                                                                c(Comma=',',
                                                                                                  Semicolon=';',
                                                                                                  Tab='\t'),
                                                                                                ','), width = 4),
                                                                            column(radioButtons("dec", "Decimal",
                                                                                                c(Dot='.',
                                                                                                  Comma=','),
                                                                                                "."), width = 4),
                                                                            column(radioButtons("wv", "Wavelength",
                                                                                                c(mm='mm',
                                                                                                  um = 'um',
                                                                                                  Wavenumber = 'wn'),
                                                                                                "mm"), width = 4)),
                                                                   hr(""),
                                                                   actionButton("spectra_plot_action", "Plot spectra"),
                                                                 ),
                                                                 h4("Model selection"),
                                                                 wellPanel(
                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            selectInput("model", "Model:", choices = c("Chl (Canvender-Bares et al. ###)" = "to_send.rda",
                                                                                                                       "LMA (Serbin et al. 2019)" = "Serbin_2019.rda"))
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
                                                                                      plotOutput('spectra_plot', height = '563px'),
                                                                                      verbatimTextOutput("user_spectra_plot"),
                                                                                      align = "center"),

                                                                             #Plot predicted leaf traits
                                                                             tabPanel("Predicted leaf trait", verbatimTextOutput("predicted_values")),

                                                                             #Summary report for predicted leaf traits
                                                                             tabPanel("Summary", tableOutput("summary"))
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

  #-----------------------------------------------------------------------------
  #Observe----------------------------------------------------------------------
  observeEvent(input$app, {
    updateTabsetPanel(session, "predict",
                      selected = "goal1")
  })

  #-----------------------------------------------------------------------------
  #Inputs-----------------------------------------------------------------------
  #Load spectra data
  spectra_load <- function(input) {

    inFile <- input$spectra_input

    #load dataset
    if (!is.null(inFile)) {
      #spectra
      spectra_frame <- read.csv(inFile$datapath, header = T, check.names = FALSE, sep = input$sep, dec = input$dec)

      #wavelength
      wavelength <- as.number(as.character(colnames(spectra_frame)[-1]))

      if(input$wv == "um") {
        wavelength <- wavelength*100
        colnames(spectra_frame)[-1] <- wavelength
      }

      if(input$wv == "wn") {
        wavelength <- 10000000/wavelength
        colnames(spectra_frame)[-1] <- wavelength
      }
    }

    return(list(frame = spectra_frame,
                wavelength = wavelength))
  }

  #Load trait data
  trait_frame <- function(input) {

    inFile <- input$trait_input

    #load dataset
    if (!is.null(inFile)) {
      trait <- read.csv(inFile$datapath, header = T, check.names = FALSE, sep = input$sep, dec = input$dec)
    }

    return(trait)
  }

  #-----------------------------------------------------------------------------
  #Functions for functionality--------------------------------------------------

  #Predict values
  predict_values <- function(spectra_input = NULL, coefficients = NULL) {

    if(!is.null(coefficients)) {
      PLSR_coef <- source(paste0("data/coefficients"))
    }

    if(!is.null(spectra_input)) {
      #Load
      spectra_frame <- spectra_load(input)
    }

  }

  #-----------------------------------------------------------------------------
  #Functions for vizualization--------------------------------------------------

  #Spectra display
  spectra_figure <- function(input) {

    #Load
    spectra_frame <- spectra_load(input)

    #Melt to plot each spectrum
    frame_melt <- spectra_frame %>% reshape2::melt(id.vars = "ID",
                                                   variable = "Wavelength",
                                                   value.name = "Reflectance")

    #Transform to number
    frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

    #Get spectra summary
    frame_summary <- frame_melt %>%
      group_by(Wavelength) %>%
      summarize(mean = mean(Reflectance),
                sd = sd(Reflectance),
                min = min(Reflectance),
                max = max(Reflectance))

    #Transform to number
    frame_summary$Wavelength <- as.numeric(as.character(frame_summary$Wavelength))

    #X limits
    x_limits <- range(frame_summary$Wavelength)

    #Plotting element
    plot <- ggplot() +
      geom_line(data  = frame_melt,
                aes(x = Wavelength, y = Reflectance, group = ID),
                colour = "grey80") +
      geom_line(data  = frame_summary,
                aes(x = Wavelength, y = min),
                colour = "red", linetype = "dashed") +
      geom_line(data  = frame_summary,
                aes(x = Wavelength, y = max),
                colour = "red", linetype = "dashed") +
      geom_line(data  = frame_summary,
                aes(x = Wavelength, y = mean),
                colour = "#0097a7ff") +
      xlab("Reflectance") + ylab("Wavelength (nm)") +
      scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      theme_bw(base_size = 14) +
      theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

    return(plot)

  }

  #-----------------------------------------------------------------------------
  #Outputs----------------------------------------------------------------------
  # Plot spectra
  output$spectra_plot <- renderPlot({
    if(!is.null(input$spectra_input)) {
      spectra_figure(input = input$spectra_input)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

################################################################################
#               ___________________________________________________            #
#                                   SpecTraits                                 #
#                               Spectra - Traits                               #
#               A Shiny Application for prediction leaf traits                 #
#                             from spectra models using R                      #
#               ___________________________________________________            #
#                                                                              #
################################################################################

# Name convention
# _input: all ASCII that serve as input
# _frame: all ASCII file after reading
# _figure: all functions to create plots
# _plot: all output that are figures

################################################################################
#Load libraries-----------------------------------------------------------------
library(shiny)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(shinythemes)

################################################################################
#Options------------------------------------------------------------------------
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# ShinyServer-------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  ###---Inputs---###------------------------------------------------------------
  
  #Load spectra data
  spectra_frame1 <- reactive({
    
    #load dataset
    spectra <- read.csv(req(input$spectra)$datapath, header = T, check.names = FALSE)
    
    #Wavelength information
    wavelength <- as.numeric(colnames(spectra)[-1])
    
    return(list(spectra = spectra, 
                wavelength = wavelength))
    
  })
  
  #Load spectra data
  spectra_load <- function(spectra_input) {
    
    inFile <- spectra_input
    
    #load dataset
    if (!is.null(inFile)){
        spectra <- read.csv(inFile$datapath, header = T, check.names = FALSE)
    }
    
    return(spectra)
  }
  
  #Load trait data
  trait_frame <- reactive({
    
    #load dataset
    trait <- read.csv(req(input$trait)$datapath, header = T, check.names = FALSE)
    
    return(trait)
    
  })
  
  ###---Functions---###---------------------------------------------------------
  
  #Spectra plot
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
      scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      theme_classic()
    
    return(plot)
    
  }
  
  ###---Outputs---###-----------------------------------------------------------
  # Plot spectra
  observeEvent("newplot", {
    output$spectra_plot <- renderPlot( 
      {
       spectra_figure(input = input$spectra_input)
      })
    })
})
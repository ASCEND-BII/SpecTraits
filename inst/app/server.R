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
})
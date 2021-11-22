#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load libraries-----------------------------------------------------------------
library(shiny)
library(data.table)

#Options
# server.r
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

# ShinyServer-------------------------------------------------------------------
shinyServer(function(input, output) {
  
  #Load spectra data
  spectra_frame <- reactive({
    
    #load dataset
    spectra <- fread(input$spectra)
    
    #Wavelength information
    wavelength <- as.numeric(colnames(spectra)[-1])
    
    return(list(spectra = spectra, 
                wavelength = wavelength))
    
  })
  
  #Load trait data
  trait_frame <- reactive({
    
    #load dataset
    trait <- fread(input$trait)
    
    return(trait)
    
  })
  
})



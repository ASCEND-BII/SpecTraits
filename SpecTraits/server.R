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

#Load defined functions---------------------------------------------------------

# ShinyServer-------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Load user defined spectra to predict leaf traits
  load_spectra <- function(input) {
    
    #load dataset
    spectra <- fread("data/spectra.csv")
    
    #Wavelength information
    wavelength <- as.numeric(colnames(spectra)[-1])
    
    return(list(spectra = spectra, 
                wavelength = wavelength))
  }
  
  
})

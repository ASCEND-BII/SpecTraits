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
#File size upload
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

################################################################################
# Source of helpers-------------------------------------------------------------
################################################################################

#import function
source("frame_import.R")

#figure
source("spectra_plot.R")
source("predicted_plot.R")

#functionality
source("predict_traits.R")
source("match_range.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ###Frames
  #Spectra to import
  spectra_frame <- import_server("spectra_import", stringsAsFactors = FALSE)

  #Leaf traits to import
  traits_frame <- import_server("traits_import", stringsAsFactors = FALSE)

  #Return table
  output$spectra_table <- renderDataTable({
    spectra_frame()
  })

  ###Plots modules
  #Return plot spectra
  callModule(spectra_plot_server,
             "spectra_figure",
             data = spectra_frame)

  observeEvent(input$refresh, {

    if(input$model != "no_apply") {

      predicted_values <- predict_traits(spectra_frame,
                                         model = input$model)

    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

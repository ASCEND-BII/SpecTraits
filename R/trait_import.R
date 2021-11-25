################################################################################
##### Import trait function
################################################################################

#Load trait data
trait_import <- function(input_path = NULL) {

  inFile <- input_path

  #load dataset
  if (!is.null(inFile)) {
    trait <- read.csv(inFile, header = T, check.names = FALSE, sep = input$sep, dec = input$dec)
  }

  return(trait)
}

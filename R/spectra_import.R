################################################################################
##### Load spectra function
################################################################################

#Load spectra data
spectra_import <- function(input_path = NULL, units = NULL) {

  inFile <- input_path

  #load dataset
  if (!is.null(inFile)) {
    #spectra
    spectra_frame <- read.csv(inFile, header = T, check.names = FALSE, sep = input$sep, dec = input$dec)

    #wavelength
    wavelength <- as.number(as.character(colnames(spectra_frame)[-1]))

  }

  #Transform units
  if(input$wv == "um") {
    wavelength <- wavelength*100
    colnames(spectra_frame)[-1] <- wavelength
  }

  if(input$wv == "wn") {
    wavelength <- 10000000/wavelength
    colnames(spectra_frame)[-1] <- wavelength
  }

  return(list(frame = spectra_frame,
              wavelength = wavelength))
}

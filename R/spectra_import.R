################################################################################
##### Load spectra function
################################################################################

#Load spectra data
spectra_import <- function(input_path = NULL, units = NULL) {

  #load dataset
  if (!is.null(input_path)) {
    #spectra
    spectra_frame <- read.csv(input_path, header = T, check.names = FALSE, sep = input$sep, dec = input$dec)

    #wavelength
    wavelength <- as.number(as.character(colnames(spectra_frame)[-1]))

    #Transform units
    if(units == "um") {
      wavelength <- wavelength*100
      colnames(spectra_frame)[-1] <- wavelength
    }

    if(units == "wn") {
      wavelength <- 10000000/wavelength
      colnames(spectra_frame)[-1] <- wavelength
    }

    return(list(frame = spectra_frame,
                wavelength = wavelength))

  }
}

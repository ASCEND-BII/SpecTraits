################################################################################
##### Load spectra function
################################################################################

#Load spectra data
spectra_import <- function(input = NULL, sep, dec, wv) {

  if (is.null(input)) {
    return(NULL)
  }

  ###load dataset
  #spectra
  spectra_frame <- read.csv(input$datapath,
                            header = T,
                            check.names = FALSE,
                            sep = sep,
                            dec = dec)

  #wavelength
  wavelength <- as.numeric(as.character(colnames(spectra_frame)[-1]))

  #Transform units
  if(wv == "um") {
    wavelength <- wavelength*100
    colnames(spectra_frame)[-1] <- wavelength
  }

  if(wv == "wn") {
    wavelength <- 10000000/wavelength
    colnames(spectra_frame)[-1] <- wavelength
  }

  return(spectra_frame)

}

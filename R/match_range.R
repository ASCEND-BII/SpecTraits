################################################################################
##### Match spectral range
################################################################################

match_range <- function(spectra_frame, range_spectra) {

  #Get range
  wv <- as.numeric(colnames(spectra_frame)[-1])

  #Error if bands are not available
  if(min(wv) >= range_spectra[1] | max(wv) <= range_spectra[2]) {

    stop("The uploaded spectra does not match the spectral
         range required to predict leaf tratis")
  }

  #Current ranges
  lower_range <- wv >= range_spectra[1]
  upper_range <- wv <= range_spectra[2]

  to_subset <- lower_range & upper_range
  to_subset <- c(FALSE, to_subset) #add ID as FALSE

  #Subset user spectral range
  spectra <- spectra_frame[, to_subset]

  return(spectra)

}

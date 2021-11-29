################################################################################
##### Spectra transpose
################################################################################

#Spectra transpose
spectra_transpose <- function(spectra_frame) {

  frame <- dcast(melt(spectra_frame, id.vars = "ID"),
                 variable ~ ID)
  colnames(frame)[1] <- "wavelength"
  frame$wavelength <- as.numeric(as.character(frame$wavelength))

  return(frame)

}

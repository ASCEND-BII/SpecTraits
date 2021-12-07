################################################################################
##### Predict values function
################################################################################

################################################################################
#Function
predict_traits <- function(spectra_frame = NULL, coeff = NULL) {

  frame <- spectra_frame()

  if(model == "Serbin_2019") {

    range_spectra <- range(coeff$coefficients$wavelength)

    #Match
    spectra <- match_range(frame, range_spectra)

    #Predict values
    predicted <- as.matrix(spectra) %*% coeff$coefficients[,2]
    predicted <- rowSums(predicted) + coeff$intercept
    predicted <- predicted^2

  }

  frame <- data.frame(ID = spectra_frame$ID,
                      predicted = predicted)

  return(frame)

}

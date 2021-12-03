################################################################################
##### Predict values function
################################################################################

################################################################################
#Function
predict_traits <- function(spectra_frame = NULL, model = NULL) {

  frame <- spectra_frame()

  if(model == "Serbin_2019") {

    data("Serbin_2019.rda")

    range_spectra <- range(Serbin_etal_2019()$coefficients$wavelength)

    #Match
    spectra <- match_range(frame, range_spectra)

    #Predict values
    predicted <- as.matrix(spectra) %*% Serbin_etal_2019$coefficients[,2]
    predicted <- rowSums(predicted) + Serbin_etal_2019$intercept
    predicted <- predicted^2

  }

  frame <- data.frame(ID = spectra_frame$ID,
                      predicted = predicted)

  return(frame)

}

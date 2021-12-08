################################################################################
##### Predict traits function
################################################################################

################################################################################
#Function
traits_predict <- function(spectra_frame, coeff, model) {

  #Spectra
  frame <- spectra_frame

  #Coefficients and intercept
  coefficients <- coeff
  intercept <- coefficients[1, 2]
  coefficients <- coefficients[-1, ]

  #Range coefficients
  range_coeff <- range(coefficients[,1])

  if(model == "Serbin_2019") {

    #Match
    spectra <- match_range(frame, range_coeff)

    #Predict values
    predicted <- as.matrix(spectra) %*% coefficients[,2]
    predicted <- rowSums(predicted) + intercept
    predicted <- predicted^2

  }

  frame <- data.frame(ID = spectra_frame[,1],
                      predicted = predicted)

  return(frame)

}

################################################################################
##### Predict traits function on build
################################################################################

################################################################################
#Function
plsr_predict <- function(spectra_frame, coefficients) {

  #Spectra
  spectra <- spectra_frame
  ID <- spectra_frame$ID

  #Coefficients and intercept
  model <- coefficients$model
  coeff <- coefficients[, -c(1:2)]
  intercept <- as.numeric(as.matrix(coefficients[, 2]))

  # March columns
  match_bands <- match(colnames(coeff), colnames(spectra))

  # Bands
  spectra <- as.matrix(spectra[, ..match_bands])

  # Collector for predictions
  predicted_iterations <- data.table()

  # Predict values
  for (ii in 1:nrow(coeff)) {

    # Perform prediction
    predicted <- spectra %*% as.numeric(coeff[ii, ])
    predicted <- predicted[,1] + intercept[ii]
    predicted_iterations <- cbind(predicted_iterations, predicted)

  }

  colnames(predicted_iterations) <- model


  frame <- cbind(data.table(ID = ID),
                 predicted_iterations)

  return(frame)

}

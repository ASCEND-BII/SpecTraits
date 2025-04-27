################################################################################
##### Predict traits function
################################################################################

################################################################################
#Function
plsr_traits_predict <- function(spectra_frame, coefficients) {

    #Spectra
    spectra <- spectra_frame

    #Coefficients and intercept
    coeff <- coefficients[, -1]
    intercept <- as.numeric(as.matrix(coefficients[, 1]))

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

    colnames(predicted_iterations) <- paste0("iteration_", 1:length(intercept))


    frame <- cbind(data.table(ID = as.numeric(as.matrix(spectra_frame[,1]))),
                              predicted_iterations)

    return(frame)

}

# spectra_frame <- fread("inst/extdata/spectra.csv")
# coefficients <- fread("inst/extdata/plsr_coefficients.csv")

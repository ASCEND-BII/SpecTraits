################################################################################
##### Predict traits function
################################################################################

################################################################################
#Function
traits_predict <- function(spectra_frame, coefficients, model) {

    #Spectra
    frame <- spectra_frame

    #Coefficients and intercept
    coefficients <- coefficients
    intercept <- coefficients[1, 2]
    coefficients <- coefficients[-1, ]

    #Range coefficients
    range_coeff <- range(coefficients[, 1])

    #Match
    spectra <- match_range(frame, range_coeff)

    #Predict values
    predicted <- as.matrix(spectra) %*% coefficients[,2]
    predicted <- rowSums(predicted) + intercept

    if(model == "Serbin et al. (2019)") {
       predicted <- predicted^2
    }

    frame <- data.frame(ID = spectra_frame[,1],
                        predicted = predicted)

    return(frame)

}


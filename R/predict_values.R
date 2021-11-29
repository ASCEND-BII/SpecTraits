################################################################################
##### Predict values function
################################################################################

source("spectra_transpose.R")

#Predict values
predict_values <- function(spectra_frame = NULL, model = NULL) {

  if(is.null(spectra_frame)) {
    return(NULL)
  }

  if(model == "Serbin_2019") {

    #load data
    load("data/Serbin_2019.rda")

    range_spectra <- range(Serbin_etal_2019$coefficients$wavelength)

    frame <- spectra_transpose(spectra_frame)

    #Match bands
    frame <- subset(frame, wavelength >= range_spectra[1] &
                           wavelength <= range_spectra[2])

    frame <- frame[, 2:ncol(frame)]

    predict <- as.matrix(frame) * unlist(as.vector(Serbin_etal_2019$coefficients[2]))
    predict <- rowSums(predict) + Serbin_etal_2019$intercept
    predict <- predict^2

  }
}

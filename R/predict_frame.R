################################################################################
##### Predict values function
################################################################################

# spectra_frame <- read.csv("inst/extdata/spectra.csv",  header = T, check.names = FALSE)

#Predict values
predict_frame <- function(spectra_frame = NULL, model = NULL) {

  if(is.null(spectra_frame)) {
    return(NULL)
  }

  if(model == "Serbin_2019") {

    #load data
    load("data/Serbin_2019.rda")

    range_spectra <- range(Serbin_etal_2019$coefficients$wavelength)

    #Match
    spectra <- match_range(spectra_frame, range_spectra)

    #Predict values
    predicted <- as.matrix(spectra) %*% Serbin_etal_2019$coefficients[,2]
    predicted <- rowSums(predicted) + Serbin_etal_2019$intercept
    predicted <- predicted^2

  }

  frame <- data.frame(ID = spectra_frame$ID,
                      LMA = predicted)

  return(frame)

}

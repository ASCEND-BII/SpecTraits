################################################################################
##### Predict values function
################################################################################

#Predict values
predict_values <- function(spectra_input = NULL, coefficients = NULL) {

  if(!is.null(coefficients)) {
    PLSR_coef <- source(paste0("data/coefficients"))
  }

  if(!is.null(spectra_input)) {
    #Load
    spectra_frame <- spectra_load(input)
  }
}

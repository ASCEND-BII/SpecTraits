################################################################################
##### Predict traits using RTM models
################################################################################

################################################################################
#Function

rtm_traits_predict <- function(spectra_frame, rtm_model) {

  # Spectra
  spectra <- spectra_frame
  nsamples <- nrow(spectra)
  names <- as.numeric(colnames(spectra)[-1])

  spectra <- try(as.matrix(spectra[, .SD, .SDcols = as.character(400:2400)]))

  if(class(spectra) == "try-error") {
    stop("Spectra must have columns from 400 to 2400 nm at 1 nm spacing (400, 401, ..., 2400).")
  }

  # ----------------------------------------------------------------------------
  # Create initial values
  if(rtm_model[1] == "prospect_d") {

    # Invert model
    fit <- bRTM(rho~prospectd,data = spectra)

  } else if(rtm_model[1] == "prospect_5b") {

    # Invert model
    fit <- bRTM(rho~prospect5,data = spectra)

  }

  return(cbind(spectra_frame[,1], as.data.table(fit$mu)))

}

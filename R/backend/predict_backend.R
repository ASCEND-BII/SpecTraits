################################################################################
# Predict backend — pure R functions shared by Shiny modules and CLI
# Requires: data.table; ccrtm (RTM only)
################################################################################

plsr_traits_predict <- function(spectra_frame, coefficients) {
  spectra_frame <- data.table::as.data.table(spectra_frame)
  coefficients  <- data.table::as.data.table(coefficients)

  #Coefficients may carry a leading "model" label column (e.g. permutation_1, cvsegment_1)
  #ahead of "intercept" — select by name rather than position so both formats work.
  band_cols <- setdiff(colnames(coefficients), c("model", "intercept"))
  coeff     <- coefficients[, ..band_cols]
  intercept <- as.numeric(coefficients[["intercept"]])

  match_bands <- match(colnames(coeff), colnames(spectra_frame))
  spectra     <- as.matrix(spectra_frame[, ..match_bands])

  predicted_iterations <- data.table::data.table()
  for (ii in seq_len(nrow(coeff))) {
    predicted <- spectra %*% as.numeric(coeff[ii, ])
    predicted <- predicted[, 1] + intercept[ii]
    predicted_iterations <- cbind(predicted_iterations, predicted)
  }
  colnames(predicted_iterations) <- paste0("iteration_", seq_along(intercept))

  cbind(data.table::data.table(ID = as.numeric(as.matrix(spectra_frame[, 1]))),
        predicted_iterations)
}

rtm_traits_predict <- function(spectra_frame, rtm_model) {
  spectra_frame <- data.table::as.data.table(spectra_frame)

  spectra <- tryCatch(
    as.matrix(spectra_frame[, .SD, .SDcols = as.character(400:2400)]),
    error = function(e)
      stop("Spectra must have columns from 400 to 2400 nm at 1 nm spacing (400, 401, ..., 2400).")
  )

  if (rtm_model[1] == "prospect_d") {
    fit <- ccrtm::bRTM(rho ~ prospectd, data = spectra)
  } else if (rtm_model[1] == "prospect_5b") {
    fit <- ccrtm::bRTM(rho ~ prospect5, data = spectra)
  } else {
    stop("Unknown RTM model: '", rtm_model, "'. Choose from: prospect_d, prospect_5b.")
  }

  cbind(spectra_frame[, 1], data.table::as.data.table(fit$mu))
}

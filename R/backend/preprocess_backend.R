################################################################################
# Preprocessing backend — pure R functions shared by Shiny modules and CLI
# Requires: spectrolab, signal, CWT, data.table
################################################################################

spectra_resample <- function(spectra_dt, wl_start = 400, wl_end = 2400, wl_step = 10) {
  spectra_dt <- data.table::as.data.table(spectra_dt)
  id_col     <- names(spectra_dt)[1]
  new_bands  <- seq(wl_start, wl_end, by = wl_step)

  spec     <- spectrolab::as_spectra(spectra_dt, name_idx = 1)
  fwhm     <- spectrolab::make_fwhm(spec, new_bands = new_bands)
  spec_res <- spectrolab::resample(spec, new_bands = new_bands, fwhm = fwhm)

  df_res         <- as.data.frame(spec_res, fix_names = "none", metadata = TRUE)
  names(df_res)[1] <- id_col
  data.table::as.data.table(df_res)
}

spectra_smooth <- function(spectra_dt, window = 11, deriv_order = 0) {
  if (is.na(window) || window < 3)      stop("'window' must be an integer >= 3.")
  if (window %% 2 == 0)                 stop("'window' must be an odd number.")
  if (is.na(deriv_order) || deriv_order < 0) stop("'deriv_order' must be >= 0.")

  spectra_dt  <- data.table::as.data.table(spectra_dt)
  id_col      <- names(spectra_dt)[1]
  spec_cols   <- 2:ncol(spectra_dt)
  spec_matrix <- as.matrix(spectra_dt[, spec_cols, with = FALSE])

  smoothed <- t(apply(spec_matrix, 1, function(row) {
    signal::sgolayfilt(row, p = 3, n = window, m = deriv_order)
  }))

  result         <- as.data.frame(smoothed)
  names(result)  <- names(spectra_dt)[spec_cols]
  result         <- cbind(as.data.frame(spectra_dt)[, 1, drop = FALSE], result)
  names(result)[1] <- id_col
  data.table::as.data.table(result)
}

spectra_transform <- function(spectra_dt,
                               transform_type    = "norm",
                               scales            = c(1, 2, 3),
                               variance          = 1,
                               deriv_window      = 11,
                               deriv_scale_order = 1) {
  spectra_dt  <- data.table::as.data.table(spectra_dt)
  id_col      <- names(spectra_dt)[1]
  spec_cols   <- 2:ncol(spectra_dt)
  spec_matrix <- as.matrix(spectra_dt[, spec_cols, with = FALSE])

  if (transform_type == "norm") {
    transformed <- t(apply(spec_matrix, 1, function(row) row / sqrt(sum(row^2))))

  } else if (transform_type == "wavelet") {
    if (is.null(scales) || length(scales) == 0)
      stop("'scales' must be a non-empty numeric vector for wavelet transformation.")
    if (is.na(variance) || variance <= 0) stop("'variance' must be positive.")
    transformed <- CWT::cwt(t = spec_matrix, scales = scales, variance = variance,
                             summed_wavelet = TRUE, threads = 1)

  } else if (transform_type == "derivative") {
    if (is.na(deriv_window) || deriv_window < 3)   stop("'deriv_window' must be >= 3.")
    if (deriv_window %% 2 == 0)                    stop("'deriv_window' must be an odd number.")
    if (is.na(deriv_scale_order) || deriv_scale_order < 1) stop("'deriv_scale_order' must be >= 1.")
    transformed <- t(apply(spec_matrix, 1, function(row) {
      signal::sgolayfilt(row, p = 3, n = deriv_window, m = deriv_scale_order)
    }))

  } else {
    stop("Unknown transform_type: '", transform_type, "'. Choose from: norm, wavelet, derivative.")
  }

  result         <- as.data.frame(transformed)
  names(result)  <- names(spectra_dt)[spec_cols]
  result         <- cbind(as.data.frame(spectra_dt)[, 1, drop = FALSE], result)
  names(result)[1] <- id_col
  data.table::as.data.table(result)
}

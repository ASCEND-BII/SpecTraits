################################################################################
##### Predict traits using RTM models
################################################################################

################################################################################
#Function

# # ------------------------------------------------------------------------------
# # Auxiliary functions
# invertion_function_opt <- function(X,
#                                subdata,
#                                PROSPECT_version,
#                                InitValues) {
#
#   invertion <- Invert_PROSPECT_OPT(lambda = subdata$lambda,
#                                    SpecPROSPECT = subdata$SpecPROSPECT,
#                                    Refl = subdata$Refl[[X]],
#                                    Tran = NULL,
#                                    PROSPECT_version = PROSPECT_version,
#                                    Parms2Estimate = 'ALL',
#                                    InitValues = InitValues[X,])
#
#   return(invertion)
#
#
# }
#
# invertion_function <- function(X,
#                                subdata,
#                                PROSPECT_version,
#                                InitValues) {
#
#   invertion <- Invert_PROSPECT(SpecPROSPECT = subdata$SpecPROSPECT,
#                                Refl = subdata$Refl[[X]],
#                                Tran = NULL,
#                                PROSPECT_version = PROSPECT_version,
#                                Parms2Estimate = 'ALL',
#                                InitValues = InitValues[X,])
#
#   return(invertion)
#
# }
#
# # ------------------------------------------------------------------------------
# # Main function
# rtm_traits_predict <- function(spectra_frame, rtm_model) {
#
#   # Spectra
#   spectra <- spectra_frame
#   nsamples <- nrow(spectra)
#
#     # ----------------------------------------------------------------------------
#   # Adjust PROSPECT optical constants & experimental leaf optics before inversion
#   subdata <- FitSpectralData(lambda = as.integer(colnames(spectra[,-1])),
#                              Refl = t(spectra[,-1]),
#                              Tran = NULL)
#
#   # ----------------------------------------------------------------------------
#   # Create initial values
#   if(rtm_model[1] == "prospect_d") {
#
#     InitValues <- data.frame(CHL = rep(40, nsamples),
#                              CAR = rep(10, nsamples),
#                              ANT = rep(0.1, nsamples),
#                              BROWN = rep(0, nsamples),
#                              EWT = rep(0.01, nsamples),
#                              LMA = rep(0.01, nsamples),
#                              N = rep(1.5, nsamples))
#     type <- "D"
#
#   } else if(rtm_model[1] == "prospect_pro") {
#
#     InitValues <- data.frame(CHL = rep(40, nsamples),
#                              CAR = rep(10, nsamples),
#                              ANT = rep(0.1, nsamples),
#                              BROWN = rep(0, nsamples),
#                              EWT = rep(0.01, nsamples),
#                              PROT = rep(0.002, nsamples),
#                              CBC = rep(0.015, nsamples),
#                              N = rep(1.5, nsamples))
#     type <- "PRO"
#
#   }
#
#   # ----------------------------------------------------------------------------
#   # Define N prior
#   if(rtm_model[2] == "N_yes") {
#
#     # Prior estimation of N using R only
#     Nprior_R <- as.vector(Get_Nprior(lambda = subdata$lambda,
#                                      Refl = subdata$Refl)[,1])
#
#     InitValues$N <- Nprior_R
#
#   }
#
#   # ----------------------------------------------------------------------------
#   # Run invertion
#
#   if(rtm_model[3] == "opt_no") {
#
#     invertion <- lapply(X = 1:nrow(spectra),
#                         FUN = invertion_function,
#                         subdata = subdata,
#                         PROSPECT_version = type,
#                         InitValues = InitValues)
#
#     invertion <- as.data.table(do.call(rbind, invertion))
#     invertion <- cbind(spectra[,1], invertion)
#
#   } else if(rtm_model[3] == "opt_yes") {
#
#     invertion <- lapply(X = 1:nrow(spectra),
#                         FUN = invertion_function_opt,
#                         subdata = subdata,
#                         PROSPECT_version = type,
#                         InitValues = InitValues)
#
#     invertion <- as.data.table(do.call(rbind, invertion))
#     invertion <- cbind(spectra[,1], invertion)
#
#   }
#
#   return(invertion)
#
# }

rtm_traits_predict <- function(spectra_frame, rtm_model) {

  # Spectra
  spectra <- spectra_frame
  nsamples <- nrow(spectra)
  names <- as.numeric(colnames(spectra)[-1])

  if(!all.equal(names, 400:2400)) {
    stop()
  }

  spectra <- as.matrix(spectra[, -1])

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



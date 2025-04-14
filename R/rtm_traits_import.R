################################################################################
##### Predict traits using RTM models
################################################################################



################################################################################
#Function

# Auxiliary function
# Define function
invertion_function_opt <- function(X,
                               subdata,
                               PROSPECT_version,
                               InitValues,
                               Nprior_R = NULL) {

  if(is.null(Nprior_R) == FALSE) {


  }

  # Modify the prior
  initial_values <- InitValues
  initial_values$N <- Nprior_R[X]

  #
  invertion <- Invert_PROSPECT_OPT(SpecPROSPECT = subdata$SpecPROSPECT,
                                   Refl = subdata$Refl[[X]],
                                   Tran = NULL,
                                   PROSPECT_version = type,
                                   Parms2Estimate = 'ALL',
                                   InitValues = initial_values)

}

# Main function
rtm_traits_predict <- function(spectra_frame, rtm_model) {

  # Spectra
  spectra <- spectra_frame[1:10,]

  # # Test bands
  # wv <- colnames(spectra[,-1])
  # lambda <- 400:2450
  # bands_keep <- match(lambda, as.integer(wv))
  # spectra <- spectra[, .SD, .SDcols = wv[bands_keep]]

  # ----------------------------------------------------------------------------
  # Adjust PROSPECT optical constants & experimental leaf optics before inversion
  subdata <- FitSpectralData(lambda = as.integer(colnames(spectra[,-1])),
                             Refl = t(spectra[,-1]),
                             Tran = NULL)

  # ----------------------------------------------------------------------------
  # Create initial values
  if(rtm_model[1] == "prospect_d") {

    InitValues <- data.frame(CHL = 40, CAR = 10, ANT = 0.1, BROWN = 0,
                             EWT = 0.01, LMA = 0.01, N = 1.5)
    type <- 'D'

  } else if(rtm_model[1] == "prospect_pro") {

    InitValues <- data.frame(CHL = 40, CAR = 10, ANT = 0.1, BROWN = 0,
                             EWT = 0.01, PROT = 0.002, CBC = 0.015, N = 1.5)
    type <- 'PRO'

  }

  # ----------------------------------------------------------------------------
  # Define N prior
  if(rtm_model[2] == "N_yes") {

    # Prior estimation of N using R only
    Nprior_R <- as.vector(Get_Nprior(lambda = subdata$lambda,
                                     Refl = subdata$Refl)[,1])

    if(rtm_model[3] == "opt_no") {

      # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        # Perform inversion
        invert <- Invert_PROSPECT(SpecPROSPECT = subdata$SpecPROSPECT,
                                     Refl = subdata$Refl[[X]],
                                     Tran = NULL,
                                     PROSPECT_version = type,
                                     Parms2Estimate = 'ALL',
                                     InitValues = initial_values)

        return(invert)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)
      invertion <- as.data.table(do.call(rbind, invertion))
      invertion <- cbind(spectra[,1], invertion)

    } else if(rtm_model[3] == "opt_yes") {

      # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        # Perform inversion
        invert <- Invert_PROSPECT_OPT(SpecPROSPECT = subdata$SpecPROSPECT,
                                         Refl = subdata$Refl[[X]],
                                         Tran = NULL,
                                         PROSPECT_version = type,
                                         Parms2Estimate = 'ALL',
                                         InitValues = initial_values)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)
      invertion <- as.data.table(do.call(rbind, invertion))
      invertion <- cbind(spectra[,1], invertion)

    }



  } else if(rtm_model[2] == "N_no") {

    if(rtm_model[3] == "opt_no") {

      # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        # Perform inversion
        invertion <- Invert_PROSPECT(SpecPROSPECT = subdata$SpecPROSPECT,
                                     Refl = subdata$Refl[[X]],
                                     Tran = NULL,
                                     PROSPECT_version = type,
                                     Parms2Estimate = 'ALL',
                                     InitValues = initial_values)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)
      invertion <- as.data.table(do.call(rbind, invertion))
      invertion <- cbind(spectra[,1], invertion)

    } else if(rtm_model[3] == "opt_yes") {

      # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        # Perform inversion
        invertion <- Invert_PROSPECT_OPT(SpecPROSPECT = subdata$SpecPROSPECT,
                                         Refl = subdata$Refl[[X]],
                                         Tran = NULL,
                                         PROSPECT_version = type,
                                         Parms2Estimate = 'ALL',
                                         InitValues = initial_values)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)

    }

          # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        # Perform inversion
        invertion <- Invert_PROSPECT(SpecPROSPECT = subdata$SpecPROSPECT,
                                     Refl = subdata$Refl[[X]],
                                     Tran = NULL,
                                     PROSPECT_version = type,
                                     Parms2Estimate = 'ALL',
                                     InitValues = initial_values)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)
      invertion <- as.data.table(do.call(rbind, invertion))
      invertion <- cbind(spectra[,1], invertion)

    } else if(rtm_model[3] == "opt_yes") {

      # Define function
      invertion_function <- function(X,
                                     subdata,
                                     PROSPECT_version,
                                     InitValues,
                                     Nprior_R) {

        # Modify the prior
        initial_values <- InitValues
        initial_values$N <- Nprior_R[X]

        #
        invertion <- Invert_PROSPECT_OPT(SpecPROSPECT = subdata$SpecPROSPECT,
                                         Refl = subdata$Refl[[X]],
                                         Tran = NULL,
                                         PROSPECT_version = type,
                                         Parms2Estimate = 'ALL',
                                         InitValues = initial_values)

      }

      invertion <- lapply(X = 1:nrow(spectra),
                          FUN = invertion_function,
                          subdata = subdata,
                          PROSPECT_version = type,
                          InitValues = InitValues,
                          Nprior_R = Nprior_R)

    }

  }


# spectra_frame <- fread("inst/extdata/spectra.csv")




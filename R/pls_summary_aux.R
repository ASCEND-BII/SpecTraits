################################################################################
##### Extract information from pls CV approaches
################################################################################

pls_summary <- function(model, ncomp, data) {

  #-----------------------------------------------------------------------------
  # Coefficients

  # Model coefficient
  coefficients <- c("final", coef(model, ncomp, intercept = TRUE)[,,1])
  coefficients <- as.data.table(matrix(coefficients, nrow = 1))
  colnames(coefficients) <- c("model", "intercept", colnames(data)[-1])

  # Get jackknife
  jackknife_coef <- as.data.table(t(model$validation$coefficients[,,ncomp,]))
  colnames(jackknife_coef) <- colnames(data)[-1]

  # Fill intercept
  intercept <- numeric()

  # Get intercept for jackknife coefficients
  for (i in 1:length(model$validation$segments)) {

    Y <- model$model[-model$validation$segments[[i]],]$trait
    Ymeans <- mean(Y)
    X <- model$model[-model$validation$segments[[i]], -1]
    Xmeans <- colMeans(X)
    intercept[i] <- Ymeans - Xmeans %*% t(as.matrix(jackknife_coef[i,]))

  }

  jackknife_coef <- cbind(data.table(model = paste0("cvsegment_", 1:nrow(jackknife_coef)),
                                     intercept = intercept),
                          jackknife_coef)
  # Final
  coefficients <- rbind(coefficients, jackknife_coef)

  #-----------------------------------------------------------------------------
  # VIP
  vip_values <- as.data.table(vip(model))
  colnames(vip_values) <- colnames(data)[-1]
  vip_values <- vip_values[ncomp,]

  #-----------------------------------------------------------------------------
  # Return
  return(list(coefficients = coefficients,
              vip = vip_values))

}

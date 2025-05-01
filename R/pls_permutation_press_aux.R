################################################################################
##### PLSR permutation function for PRESS estimation
################################################################################

pls_permutation_press <- function(formula, maxcomp, iterations, prop, data) {

  apply_permutation_press <- function(X,
                                maxcomp,
                                iterations,
                                prop,
                                data) {

    # Subsample for permutation
    perm_sample <- sample(1:nrow(data), floor(nrow(data)*prop))
    data_perm <- data[perm_sample, ]

    # Create model
    plsr_model <- plsr(formula = trait ~ .,
                       scale = FALSE,
                       center = TRUE,
                       ncomp = maxcomp,
                       validation = "none",
                       trace = FALSE,
                       method = "oscorespls",
                       data = data_perm)

    predicted_validation <- predict(plsr_model,
                                    newdata = data)

    sqrt_residuals <- (predicted_validation[,,] - data$trait)^2
    press_results <- apply(X = sqrt_residuals, MARGIN = 2, FUN = sum)
    return(press_results)

  }

  permutations <- lapply(X = 1:iterations,
                         FUN = apply_permutation_press,
                         maxcomp = maxcomp,
                         prop = prop,
                         data = data)

  permutation_results <- do.call(rbind, permutations)
  permutation_results <- as.data.table(permutation_results)

  return(permutation_results)

}

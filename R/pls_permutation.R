################################################################################
##### PLSR permutation function
################################################################################

pls_permutation <- function(formula, maxcomp, iterations, prop, data, PRESS = TRUE) {

  apply_permutation <- function(X,
                                maxcomp,
                                iterations,
                                prop,
                                data,
                                PRESS) {

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
                       data = data_perm)

    if(isTRUE(PRESS)) {

      predicted_validation <- predict(plsr_model,
                                      newdata = data[!perm_sample, ])

      sqrt_residuals <- sqrt((predicted_validation[,,] - data[!perm_sample, ]$trait)^2)
      press_results <- apply(X = sqrt_residuals, MARGIN = 2, FUN = sum)
      return(press_results)

    } else if(isTRUE(PRESS)) {



    }
  }

  permutations <- lapply(X = 1:iterations,
                         FUN = apply_permutation,
                         maxcomp = maxcomp,
                         prop = prop,
                         data = data,
                         PRESS = TRUE)


  permutation_results <- do.call(rbind, permutations)
  permutation_results <- as.data.table(permutation_results)

  return(permutation_results)

}

################################################################################
##### PLSR permutation function for coefficients and VIP
################################################################################

pls_permutation_coef <- function(formula, maxcomp, iterations, prop, data) {

  apply_permutation_coef <- function(X,
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

    coef_values <- coef(plsr_model, ncomp = maxcomp, intercept = TRUE)[,,1]
    vip_values <- vip(plsr_model)[maxcomp,]

    return(list(coefficients = coef_values,
                vip_values = vip_values))
  }

  permutations <- lapply(X = 1:iterations,
                         FUN = apply_permutation_coef,
                         maxcomp = maxcomp,
                         prop = prop,
                         data = data)

  # rbind all coefficients
  all_coefficients <- as.data.table(do.call(rbind, lapply(permutations, function(x) x$coefficients)))
  colnames(all_coefficients) <- colnames(data)
  colnames(all_coefficients)[1] <- "intercept"
  all_coefficients <- cbind(data.table(model = paste0("permutation_", 1:iterations)),
                            all_coefficients)

  # rbind all vip
  all_vip <- as.data.table(do.call(rbind, lapply(permutations, function(x) x$vip_values)))
  colnames(all_vip) <- colnames(data)[-1]

  return(list(coefficients = all_coefficients,
              vip = all_vip))

}

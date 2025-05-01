################################################################################
##### Look for the optimal number of components
################################################################################

# Find the optimal number of components manually
find_optimal_ncomp <- function(model,
                               traits,
                               method) {

  if(method == "loo" | method == "cv") {

    # --------------------------------------------------------------------------
    # Get optimal
    press <- model$validation$PRESS
    errors <- sqrt(press / length(traits))
    best_idx <- which.min(errors)

    # 1-SE rule
    min_error <- errors[best_idx]
    se <- sd(errors) / sqrt(length(errors))
    threshold <- min_error + se
    optimal <- min(which(errors <= threshold))

    # --------------------------------------------------------------------------
    # Get errors
    # Extract predictions
    pred_array <- model$validation$pred
    n_samples <- dim(pred_array)[1]
    ncomp <- dim(pred_array)[3]

    errors_mat <- matrix(NA, nrow = n_samples, ncol = ncomp)
    for (k in 1:ncomp) {
      pred_k <- pred_array[ ,1,k]
      errors_mat[,k] <- (traits - pred_k)^2
    }

    # Mean RMSE per ncomp
    rmsep <- data.table(ncomp = 1:ncomp,
                        rmsep_mean = sqrt(colMeans(errors_mat)),
                        rmsep_sd = apply(errors_mat, 2, function(e) {sd(sqrt(e)/sqrt(length(traits)))}))

    legend <- "Error bars represent the standard error among predictions"

  } else if(method == "permutation") {

    # --------------------------------------------------------------------------
    # Get optimal
    press <- colMeans(model)
    errors <- sqrt(press / length(traits))
    best_idx <- which.min(errors)

    # 1-SE rule
    min_error <- errors[best_idx]
    se <- sd(errors) / sqrt(length(errors))
    threshold <- min_error + se
    optimal <- min(which(errors <= threshold))

    # --------------------------------------------------------------------------
    # Get errors
    # Extract predictions
    errors_mat <- sqrt(model / length(traits))

    # Mean RMSE per ncomp
    rmsep <- data.table(ncomp = 1:ncomp,
                        rmsep_mean = colMeans(sqrt(model / length(traits))),
                        rmsep_sd = apply(errors_mat, 2, function(e) {sd(e)})) #/sqrt(nrow(errors_mat))

    legend <- "Error bars represent the standard deviation among permutations"

  }

  res <- list(rmsep = rmsep,
              optimal = optimal,
              legend = legend)

  return(res)

}

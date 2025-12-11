################################################################################
##### Model performance estimation
################################################################################

model_performance <- function(observed, predicted) {

    collector <- data.table()

    # Performance
    for(i in 1:ncol(predicted)) {
      per <- parameters(obs = observed, pred = predicted[[i]])
      model <- colnames(predicted)[i]
      names(model) <- "model"
      per <- c(model, per)
      collector <- rbind(collector, per)
    }

    return(collector)
}

parameters <- function(obs, pred) {

  linear <- lm(obs ~ pred, na.action = na.exclude)
  linear_summary <- summary(linear)

  R2 <- linear_summary$r.squared
  intercept <- linear_summary$coefficients[1,1]
  slope <- linear_summary$coefficients[2,1]
  BIAS <- mean(obs-pred, na.rm = TRUE)
  RMSE <- sqrt(sum(((obs-pred)^2))/length(obs))
  RRMSE <- sqrt(sum(((obs-pred)^2))/length(obs))/(max(obs) - min(obs))
  names(RRMSE) <- NULL


  return(round(data.table(R2 = R2,
                          BIAS = BIAS,
                          RMSE = RMSE,
                          RRMSE = RRMSE,
                          intercept = intercept,
                          slope = slope), 4))

}

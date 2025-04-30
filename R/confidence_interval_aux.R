confidence_interval <- function(vec, confidence_level = 0.95) {

  n <- length(vec)
  mean_val <- mean(vec, na.rm = TRUE)
  sd_val <- sd(vec, na.rm = TRUE)
  stderr <- sd_val / sqrt(n)

  t_score <- qt((1 + confidence_level) / 2, df = n - 1)
  margin_error <- t_score * stderr

  lower_bound <- mean_val - margin_error
  upper_bound <- mean_val + margin_error

  c(lower_bound, upper_bound)

}

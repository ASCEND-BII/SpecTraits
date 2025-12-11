################################################################################
##### VIP estimation function
################################################################################

vip <- function(model) {

  SS <- c(model$Yloadings)^2 * colSums(model$scores^2)
  Wnorm2 <- colSums(model$loading.weights^2)
  SSW <- sweep(model$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))

}

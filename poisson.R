poisson.lik <- function(mu, y) {
  n <- nrow(y)
  logl <- sum(y) * log(mu) - n * mu
  return(-logl)
}

optim_logistic <- function(beta, X, Y) {
  beta <- as.matrix(beta, nrow = 4)
  pi <- plogis(X %*% beta)
  loglikelihood <- -sum(Y * log(pi) + (1 - Y) * log(1 - pi))
  return(loglikelihood)
}

poisson.lik <- function(beta, X, Y) {
  n <- nrow(Y)
  mu <- beta[1] + beta[2]*X
  mu <- ifelse(mu == 0, 0.0001, mu)
  logl <- sum(Y) * log(mu) - n * mu
  return(-logl)
}

# very important in Poisson Regression is equidispersion, which means that the mean and variance of the distribution are equal.
x <- rnorm(100)
y <- ceiling(exp(1 + 0.3*x))
glm(y ~ x, family=poisson)

mean(y)
var(y)

optim(par=c(0, 0),  fn=poisson.lik,X=x,Y=y)

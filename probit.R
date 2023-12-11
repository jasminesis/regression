################# probit regression function
probit_regression <- function(data, x1, x2, ..., y) {
  n <- nrow(data)
  x_parameters <- c(x1, x2, ...)
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  Y <- matrix(y, nrow = n, ncol = 1)
  probit.loglikelihood <- function(beta, X, Y) {
    eta <- X %*% beta
    p <- pnorm(eta)
    loglikelihood <- -sum((1 - Y) * log(1 - p) + Y * log(p))
    return(loglikelihood)
  }
  initial_guess <- matrix(0, nrow = ncol(data), ncol = 1)
  result <- optim(
    initial_guess,
    probit.loglikelihood,
    X = X,
    Y = Y,
    method = 'BFGS'
  )
  estimate <-
    c(result$par[1], result$par[2], result$par[3], result$par[4])
  num_bootstraps <- 100
  result_bootstrap <-
    matrix(0, nrow = num_bootstraps, ncol = length(estimate))
  for (i in 1:num_bootstraps) {
    sample_indices <- sample(nrow(data), replace = TRUE)
    bootstrap_data <- data[sample_indices,]
    X_bootstrap <-
      matrix(
        c(rep(1, nrow(bootstrap_data)), x_parameters),
        nrow = nrow(bootstrap_data),
        ncol = ncol(bootstrap_data)
      )
    Y_bootstrap <-
      matrix(bootstrap_data$y,
             nrow = nrow(bootstrap_data),
             ncol = 1)
    initial_guess_bootstrap <-
      matrix(0, nrow = ncol(bootstrap_data), ncol = 1)
    result_bootstrap[i, ] <- optim(
      initial_guess_bootstrap,
      probit.loglikelihood,
      X = X_bootstrap,
      Y = Y_bootstrap,
      method = 'BFGS'
    )$par
  }
  se <- apply(result_bootstrap, 2, sd)
  return(list(estimate, se))
}
# test data frame for probit regression
test_probit_regression_data <- data.frame(
  x1 = rnorm(100, 0, 2),
  x2 = rnorm(100, 4, 1),
  x3 = rnorm(100, 5, 3),
  y = rbinom(5, size = 1, prob = 0.2)
)
# applying created function to test data frame
our_implementation_probit <- probit_regression(
  test_probit_regression_data,
  x1 = test_probit_regression_data$x1,
  x2 = test_probit_regression_data$x2,
  x3 = test_probit_regression_data$x3,
  y = test_probit_regression_data$y
)
# comparing results to glm probit output
r_implementation_probit <- summary(glm(
  y ~ x1 + x2 + x3,
  data = test_probit_regression_data,
  family = binomial(link = 'probit')
))$coefficients[, 1:2]

our_implementation_probit_estimates <- unlist(our_implementation_probit)[1:4]
our_implementation_probit_se <- unlist(our_implementation_probit)[5:8]
our_implementation_probit <- data.frame(our_implementation_probit_estimates, our_implementation_probit_se)
row.names(our_implementation_probit) <- c('(Intercept)', 'x1', 'x2', 'x3')
colnames(our_implementation_probit) <- c('Estimate', 'Std. Error')
our_implementation_probit
r_implementation_probit

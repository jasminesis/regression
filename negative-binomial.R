set.seed(123)

library(tidyverse)
library(dplyr)
library(MASS)

################# negative binomial regression function
negative_binomial_regression <- function(data, ..., y) {
  n <- nrow(data)
  x_parameters <- c(...)
  # defining the predictor matrix
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  # defining the outcome matrix
  Y <- matrix(y, nrow = n, ncol = 1)
  # starting with theta = 1
  theta <- 1
  # defining the log likelihood
  negative_binomial.likelihood <- function(beta, X, Y = y) {
    eta <- X %*% beta
    mu <- exp(eta)
    loglikelihood <-
      sum(Y * log(mu) - (Y + 1 / theta) * log(1 + mu / theta))
    return(loglikelihood)
  }
  # starting with an initial guess of the parameter values
  initial_guess <- rep(0, ncol(X))
  # using 'optim' to maximize the log likelihood
  result <- optim(
    initial_guess,
    negative_binomial.likelihood,
    X = X,
    Y = Y,
    control = list(fnscale = -1),
    hessian = T,
    method = NULL
  )$par
  # creating a vector 'estimate' for the beta coefficients
  estimate <- result
  # bootstrapping to estimate the standard errors
  num_bootstraps <- 10000
  result_bootstrap <-
    matrix(0, nrow = num_bootstraps, ncol = ncol(X))
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
      negative_binomial.likelihood,
      X = X_bootstrap,
      Y = Y_bootstrap,
      control = list(fnscale = -1),
      hessian = T,
      method = NULL
    )$par
  }
  # finding the standard deviation of the bootstrapped betas to find the
  # standard error of the coefficients
  se <- apply(result_bootstrap, 2, sd)
  # calculating the z-statistic
  z <- estimate / se
  # defining the degrees of freedom
  df <- nrow(X) - ncol(X)
  # calculating the p-value
  p <- 2 * pnorm(z, lower.tail = FALSE)
  # defining the row names of the output data frame
  rownames <- c()
  for (i in 1:((ncol(X)) - 1)) {
    rownames[i] <-  i
  }
  # returning a data frame akin to the glm probit output
  return(
    data.frame(
      Estimate = estimate,
      Std.Error = se,
      z.value = z,
      p.value = p,
      DegOfFreedom = c(df, rep(NA, ncol(X) - 1)),
      row.names = c('(Intercept)', paste0(rep('x', ncol(
        X
      ) - 1), rownames))
    )
  )
}
# creating our prediction function for negative binomial regression
predict_neg_binom <-
  function(data, ..., y, implementation_neg_binom) {
    n <-
      implementation_neg_binom$DegOfFreedom[1] + nrow(implementation_neg_binom)
    input_covariate_values <- c(...)
    X <-
      matrix(
        c(rep(1, n), input_covariate_values),
        nrow = n,
        ncol = nrow(implementation_neg_binom)
      )
    Y <- matrix(y, nrow = n, ncol = 1)
    estimate <-
      implementation_neg_binom[1:nrow(implementation_neg_binom), 1]
    pred <- exp(X %*% estimate)
    return(pred)
  }
# test data frame for negative binomial regression
# to ensure there is a linear relationship between
# predictors and the log of the outcome's mean
x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(100, mean = 0, sd = 0.5)
y <- rnbinom(100, mu = exp(x1 + x2), size = 0.5)
test_neg_binom_regression_data <- data.frame(x1, x2, y)
# to ensure that the variance of the outcome variable is greater
# than its mean
var(y) > mean(y)
# applying created function to test data frame
our_implementation_neg_binom <-
  negative_binomial_regression(
    test_neg_binom_regression_data,
    test_neg_binom_regression_data$x1,
    test_neg_binom_regression_data$x2,
    y = test_neg_binom_regression_data$y
  )
our_implementation_neg_binom
# comparing results to glm.nb output
r_implementation_neg_binom <-
  summary(glm.nb(y ~ x1 + x2, data = test_neg_binom_regression_data))
r_implementation_neg_binom

################# results are similar

# we followed all assumptions of negative binomial regression in
# regressing y on x1 and x2 using the test_neg_binom_regression_data
# data set. We will compare the accuracy of this
# regression to that of all the others where assumptions
# will be broken
prediction_all_assumptions_met <-
  as.numeric(
    predict_neg_binom(
      test_neg_binom_regression_data,
      test_neg_binom_regression_data$x1,
      test_neg_binom_regression_data$x2,
      y = test_neg_binom_regression_data$y,
      implementation_neg_binom = our_implementation_neg_binom
    )
  )
residual_all_assumptions_met <- sqrt(mean((
  test_neg_binom_regression_data$y - prediction_all_assumptions_met
) ^ 2
)) # small residual
# residual plot
plot(
  test_neg_binom_regression_data$y - prediction_all_assumptions_met,
  ylim = c(-30, 30),
  ylab = 'Residuals',
  main = 'Residual Plot: All assumptions met',
  pch = 16
)
abline(
  h = 0,
  col = "red",
  lty = 2,
  lwd = 3
)

# assumption 1: there is a linear relationship between
# predictors and the log of the outcome's mean
# breaking this assumption
x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(100, mean = 0, sd = 0.5)
y <- rnbinom(100, mu = exp(x1 + x2) ^ 3, size = 0.5)
test_neg_binom_regression_data_not_linear <- data.frame(x1, x2, y)
# to ensure that the variance of the outcome variable is greater
# than its mean
var(y) > mean(y)
our_implementation_neg_binom_not_linear <-
  negative_binomial_regression(
    test_neg_binom_regression_data_not_linear,
    test_neg_binom_regression_data_not_linear$x1,
    test_neg_binom_regression_data_not_linear$x2,
    y = test_neg_binom_regression_data_not_linear$y
  )
our_implementation_neg_binom_not_linear
prediction_not_linear <-
  as.numeric(
    predict_neg_binom(
      test_neg_binom_regression_data_not_linear,
      test_neg_binom_regression_data_not_linear$x1,
      test_neg_binom_regression_data_not_linear$x2,
      y = test_neg_binom_regression_data_not_linear$y,
      implementation_neg_binom = our_implementation_neg_binom_not_linear
    )
  )
residual_not_linear <- sqrt(mean((
  test_neg_binom_regression_data_not_linear$y - prediction_not_linear
) ^ 2
)) # large residual
# residual plot
plot(
  test_neg_binom_regression_data_not_linear$y - prediction_not_linear,
  ylim = c(-30, 30),
  ylab = 'Residuals',
  main = 'Residual Plot: Linearity aasumption violated',
  pch = 16
)
abline(
  h = 0,
  col = 'red',
  lty = 2,
  lwd = 3
)

# assumption 2: variance of the outcome variable is greater
# than its mean
# breaking this assumption
x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(100, mean = 0, sd = 0.5)
y <- rnbinom(100, mu = 5, size = 20)
test_neg_binom_regression_data_mean_greater <- data.frame(x1, x2, y)
# to ensure that the variance of the outcome variable is smaller
# than its mean
var(y) > mean(y)
our_implementation_neg_binom_mean_greater <-
  negative_binomial_regression(
    test_neg_binom_regression_data_mean_greater,
    test_neg_binom_regression_data_mean_greater$x1,
    test_neg_binom_regression_data_mean_greater$x2,
    y = test_neg_binom_regression_data_mean_greater$y
  )
our_implementation_neg_binom_mean_greater
prediction_mean_greater <-
  as.numeric(
    predict_neg_binom(
      test_neg_binom_regression_data_mean_greater,
      test_neg_binom_regression_data_mean_greater$x1,
      test_neg_binom_regression_data_mean_greater$x2,
      y = test_neg_binom_regression_data_mean_greater$y,
      implementation_neg_binom = our_implementation_neg_binom_mean_greater
    )
  )
residual_mean_greater <- sqrt(mean((
  test_neg_binom_regression_data_mean_greater$y - prediction_mean_greater
) ^ 2
)) # large residual
# residual plot
plot(
  test_neg_binom_regression_data_mean_greater$y - prediction_mean_greater,
  ylim = c(-30, 30),
  ylab = 'Residuals',
  cex.main = 0.9,
  main = 'Residual Plot: Variance of outcome greater than mean assumption violated',
  pch = 16
)
abline(
  h = 0,
  col = "red",
  lty = 2,
  lwd = 3
)

# comparing all the residuals
residual_comparison <-
  t(
    data.frame(
      residual_all_assumptions_met,
      residual_not_linear,
      residual_mean_greater
    )
  )
row.names(residual_comparison) <- c(
  'All assumptions met',
  'Linearity assumption violated',
  'Variance > Mean assumption violated'
)
colnames(residual_comparison) <- 'Residuals'
residual_comparison

# Conclusion:
# The implementation of negative binomial regression
# where all assumptions are met performs
# well; however, even the model where an assumption is
# broken; i.e. where the mean
# of the outcome is greater than its variance,
# performs well too

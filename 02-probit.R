set.seed(123)

library(tidyverse)
library(dplyr)

################# probit regression function
probit_regression <- function(data, ..., y) {
  n <- nrow(data)
  x_parameters <- c(...)
  # defining the predictor matrix
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  # defining the outcome matrix
  Y <- matrix(y, nrow = n, ncol = 1)
  # defining the log likelihood
  probit.loglikelihood <- function(beta, X, Y) {
    eta <- X %*% beta
    p <- pnorm(eta)
    loglikelihood <- -sum((1 - Y) * log(1 - p) + Y * log(p))
    return(loglikelihood)
  }
  # starting with an initial guess of the parameter values
  initial_guess <- matrix(0, nrow = ncol(data), ncol = 1)
  # using 'optim' to maximize the log likelihood
  result <- optim(
    initial_guess,
    fn = probit.loglikelihood,
    X = X,
    Y = Y,
    method = 'BFGS',
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
      probit.loglikelihood,
      X = X_bootstrap,
      Y = Y_bootstrap,
      method = 'BFGS'
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
# creating our prediction function for probit regression
predict_probit <-
  function(data, ..., y, implementation_probit) {
    n <-
      implementation_probit$DegOfFreedom[1] + nrow(implementation_probit)
    input_covariate_values <- c(...)
    X <-
      matrix(
        c(rep(1, n), input_covariate_values),
        nrow = n,
        ncol = nrow(implementation_probit)
      )
    Y <- matrix(y, nrow = n, ncol = 1)
    estimate <-
      implementation_probit[1:nrow(implementation_probit), 1]
    pred <- ifelse(X %*% estimate < 0, 0, 1)
    return(pred)
  }
# test data frame for probit regression
# to ensure there is a linear relationship between
# x and the z score of y
test_probit_regression_data <- data.frame(x1 = rnorm(1000, 0, 1),
                                          x2 = rnorm(1000, 0, 1))
test_probit_regression_data$y <- test_probit_regression_data$x1 +
  rnorm(1000, mean = 0, sd = 0.5)
test_probit_regression_data$y <-
  qnorm(pnorm(test_probit_regression_data$y))
test_probit_regression_data$y <-
  ifelse(test_probit_regression_data$y < 0, 0, 1)
# applying created function to test data frame
our_implementation_probit <-
  probit_regression(
    test_probit_regression_data,
    test_probit_regression_data$x1,
    test_probit_regression_data$x2,
    y = test_probit_regression_data$y
  )
our_implementation_probit
# comparing results to glm probit output
r_implementation_probit <-
  summary(glm(y ~ x1 + x2, data = test_probit_regression_data,
              family = binomial(link = 'probit')))
r_implementation_probit

################# results are similar

# we followed all assumptions of probit regression in
# regressing y on x1 and x2 using the test_probit_regression_data
# data set. We will compare the accuracy of this
# regression to that of all the others where assumptions
# will be broken
prediction_all_assumptions_met <-
  as.numeric(
    predict_probit(
      test_probit_regression_data,
      test_probit_regression_data$x1,
      test_probit_regression_data$x2,
      y = test_probit_regression_data$y,
      implementation_probit = our_implementation_probit
    )
  )
accuracy_all_assumptions_met <-
  sum(prediction_all_assumptions_met == test_probit_regression_data$y)
accuracy_all_assumptions_met # high accuracy here

# assumption 1: there is a linear relationship between
# x and the z score of y
# breaking this assumption
test_probit_regression_data_not_linear <-
  data.frame(x1 = rnorm(1000, 0, 1),
             x2 = rnorm(1000, 0, 1))
test_probit_regression_data_not_linear$y <-
  test_probit_regression_data_not_linear$x1 ^ 2 +
  rnorm(1000, mean = 0, sd = 0.5)
test_probit_regression_data_not_linear$y <-
  qnorm(pnorm(test_probit_regression_data_not_linear$y))
test_probit_regression_data_not_linear$y <-
  ifelse(test_probit_regression_data_not_linear$y < 0, 0, 1)
our_implementation_probit_not_linear <-
  probit_regression(
    test_probit_regression_data_not_linear,
    test_probit_regression_data_not_linear$x1,
    test_probit_regression_data_not_linear$x2,
    y = test_probit_regression_data_not_linear$y
  )
our_implementation_probit_not_linear
prediction_not_linear <-
  as.numeric(
    predict_probit(
      test_probit_regression_data_not_linear,
      test_probit_regression_data_not_linear$x1,
      test_probit_regression_data_not_linear$x2,
      y = test_probit_regression_data_not_linear$y,
      implementation_probit = our_implementation_probit_not_linear
    )
  )
accuracy_not_linear <-
  sum(prediction_not_linear == test_probit_regression_data_not_linear$y)
accuracy_not_linear # lower accuracy here

# assumption 2: errors are normally distributed
# (and are independent but we will not be able to show this using code)
# breaking this assumption
test_probit_regression_data_not_normally_dist <-
  data.frame(x1 = rnorm(1000, 0, 1),
             x2 = rnorm(1000, 0, 1))
test_probit_regression_data_not_normally_dist$y <-
  test_probit_regression_data_not_normally_dist$x1 +
  runif(1000, min = -1, max = 1)
test_probit_regression_data_not_normally_dist$y <-
  qnorm(pnorm(test_probit_regression_data_not_normally_dist$y))
test_probit_regression_data_not_normally_dist$y <-
  ifelse(test_probit_regression_data_not_normally_dist$y < 0, 0, 1)
our_implementation_probit_not_normally_dist <-
  probit_regression(
    test_probit_regression_data_not_normally_dist,
    test_probit_regression_data_not_normally_dist$x1,
    test_probit_regression_data_not_normally_dist$x2,
    y = test_probit_regression_data_not_normally_dist$y
  )
our_implementation_probit_not_normally_dist
prediction_not_normally_dist <-
  as.numeric(
    predict_probit(
      test_probit_regression_data_not_normally_dist,
      test_probit_regression_data_not_normally_dist$x1,
      test_probit_regression_data_not_normally_dist$x2,
      y = test_probit_regression_data_not_normally_dist$y,
      implementation_probit = our_implementation_probit_not_normally_dist
    )
  )
accuracy_not_normally_dist <-
  sum(prediction_not_normally_dist == test_probit_regression_data_not_normally_dist$y)
accuracy_not_normally_dist # lower accuracy here

# comparing all the accuracies
accuracy_comparison <-
  t(
    data.frame(
      accuracy_all_assumptions_met,
      accuracy_not_linear,
      accuracy_not_normally_dist
    )
  )
row.names(accuracy_comparison) <- c(
  'All assumptions met',
  'Linearity assumption violated',
  'Normality assumption violated'
)
colnames(accuracy_comparison) <- 'Accuracy'
accuracy_comparison

# Conclusion:
# The implementation of probit regression
# where all assumptions are met performs
# the best; i.e. it gives us predictions which
# are more accurate to the true outcome values

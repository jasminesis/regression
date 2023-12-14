set.seed(123)

library(tidyverse)
library(dplyr)

################# linear regression function
linear_regression <- function(data, ..., y) {
  x_parameters <- c(...)
  n <- nrow(data)
  # defining the predictor matrix
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  # defining the outcome matrix
  Y <- matrix(y, nrow = n, ncol = 1)
  # solving for the beta coefficients
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  # creating a vector 'estimate' for the beta coefficients
  estimate <- c()
  for (i in 1:ncol(X)) {
    estimate[i] <- beta[i]
  }
  # bootstrapping to estimate the standard errors
  num_bootstraps <- 10000
  bootstrap_betas <-
    matrix(0, nrow = num_bootstraps, ncol = ncol(data))
  for (i in 1:num_bootstraps) {
    sample_indices <- sample(nrow(data), replace = TRUE)
    bootstrap_data <- data[sample_indices, ]
    bootstrap_X <-
      as.matrix(cbind(1, bootstrap_data[, 1:(ncol(bootstrap_data) - 1)]))
    bootstrap_Y <- as.matrix(bootstrap_data$y, ncol = 1)
    bootstrap_beta <-
      solve(t(bootstrap_X) %*% bootstrap_X) %*% t(bootstrap_X) %*% bootstrap_Y
    bootstrap_betas[i, ] <- bootstrap_beta
  }
  # finding the standard deviation of the bootstrapped betas to find the
  # standard error of the coefficients
  se <- c()
  for (i in 1:ncol(X)) {
    se[i] <- apply(bootstrap_betas, 2, sd)[i]
  }
  # calculating the t-statistic
  t <- estimate / se
  # defining the degrees of freedom
  df <- nrow(X) - ncol(X)
  # calculating the p-value
  p <- 2 * pt(t, df, lower = F)
  # calculating the residuals
  residual <- sqrt(mean((Y - X %*% beta) ^ 2))
  # defining the row names of the output data frame
  rownames <- c()
  for (i in 1:((ncol(X)) - 1)) {
    rownames[i] <-  i
  }
  # returning a data frame akin to the lm output
  return(
    data.frame(
      Estimate = estimate,
      Std.Error = se,
      t.value = t,
      p.value = p,
      Residual = c(residual, rep(NA, ncol(X) - 1)),
      DegOfFreedom = c(df, rep(NA, ncol(X) - 1)),
      row.names = c('(Intercept)', paste0(rep('x', ncol(
        X
      ) - 1), rownames))
    )
  )
}
# test data frame for linear regression
test_linear_regression_data <-
  data.frame(x1 = rnorm(100, mean = 5, sd = 2),
             x2 = rnorm(100, mean = 0, sd = 2))
test_linear_regression_data$y <-
  2 * test_linear_regression_data$x1 +
  0.2 * test_linear_regression_data$x2 + rnorm(100, mean = 3, sd = 2)
# applying created function to test data frame
our_implementation <- linear_regression(
  test_linear_regression_data,
  test_linear_regression_data$x1,
  test_linear_regression_data$x2,
  y = test_linear_regression_data$y
)
our_implementation
# comparing results to lm output
r_implementation <-
  summary(lm(y ~ x1 + x2, data = test_linear_regression_data))
r_implementation

################# results are similar

# we followed all assumptions of linear regression in
# regressing y on x1 and x2 using the test_linear_regression_data
# data set. We will compare the residual of this
# regression to that of all the others where assumptions
# will be broken
our_implementation$Residual[1] # a small residual here

# assumption 1: x and y follow a linear relationship
# breaking this assumption
test_linear_regression_data_not_linear <-
  data.frame(x1 = rnorm(100, mean = 5, sd = 2),
             x2 = rnorm(100, mean = 0, sd = 2))
test_linear_regression_data_not_linear$y <-
  2 * test_linear_regression_data_not_linear$x1 ^ 2 + 0.2 *
  test_linear_regression_data_not_linear$x2 ^ 2 + rnorm(100, mean = 3, sd = 2)
our_implementation_not_linear <- linear_regression(
  test_linear_regression_data_not_linear,
  test_linear_regression_data_not_linear$x1,
  test_linear_regression_data_not_linear$x2,
  y = test_linear_regression_data_not_linear$y
)
our_implementation_not_linear$Residual[1] # a higher residual here

# assumption 2: errors are normally distributed
# (and are independent but we will not be able to show this using code)
# breaking this assumption
test_linear_regression_data_not_normally_dist <-
  data.frame(x1 = rnorm(100, mean = 5, sd = 2),
             x2 = rnorm(100, mean = 0, sd = 2))
test_linear_regression_data_not_normally_dist$y <-
  2 * test_linear_regression_data_not_normally_dist$x1 + 0.2 *
  test_linear_regression_data_not_normally_dist$x2 + runif(100, min = 10, max = 20)
our_implementation_not_normally_dist <- linear_regression(
  test_linear_regression_data_not_normally_dist,
  test_linear_regression_data_not_normally_dist$x1,
  test_linear_regression_data_not_normally_dist$x2,
  y = test_linear_regression_data_not_normally_dist$y
)
our_implementation_not_normally_dist$Residual[1] # a higher residual here

# assumption 3: errors are homoscedastic
# breaking this assumption
test_linear_regression_data_not_homoscedastic <-
  data.frame(x1 = rnorm(100, mean = 5, sd = 2),
             x2 = rnorm(100, mean = 0, sd = 2))
test_linear_regression_data_not_homoscedastic$y <-
  2 * test_linear_regression_data_not_homoscedastic$x1 + 0.2 *
  test_linear_regression_data_not_homoscedastic$x2 + rnorm(100, mean = 2, sd = 1) +
  rnorm(100, mean = 2, sd = 5)
our_implementation_not_homoscedastic <- linear_regression(
  test_linear_regression_data_not_homoscedastic,
  test_linear_regression_data_not_homoscedastic$x1,
  test_linear_regression_data_not_homoscedastic$x2,
  y = test_linear_regression_data_not_homoscedastic$y
)
our_implementation_not_homoscedastic$Residual[1] # a higher residual here

# comparing all the residuals
residual_comparison <-
  t(
    data.frame(
      resid_all_assumptions_met = our_implementation$Residual[1],
      resid_not_linear = our_implementation_not_linear$Residual[1],
      resid_not_normally_dist = our_implementation_not_normally_dist$Residual[1],
      resid_not_homoscedastic = our_implementation_not_homoscedastic$Residual[1]
    )
  )
row.names(residual_comparison) <- c(
  'All assumptions met',
  'Linearity assumption violated',
  'Normality assumption violated',
  'Homoscedasticity assumption violated'
)
colnames(residual_comparison) <- 'Residuals'
residual_comparison

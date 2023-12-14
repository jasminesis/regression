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
    method = NULL,
  )$par
  # creating a vector 'estimate' for the beta coefficients
  estimate <- result
  # bootstrapping to estimate the standard errors
  num_bootstraps <- 100
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
  p <- 2 * (1 - pnorm(z))
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

# to ensure there is a linear relationship between
# x1, x2 and y
# test_probit_regression_data$y <-
#   c(
#     ifelse(test_probit_regression_data$x2[1:100] < 0,
#            0, 1),
#     ifelse(test_probit_regression_data$x1[101:1000] < 0,
#            1, 0)
#   )
# applying created function to test data frame
# our_implementation_probit <-
#   probit_regression(
#     test_probit_regression_data,
#     test_probit_regression_data$x1,
#     test_probit_regression_data$x2,
#     y = test_probit_regression_data$y
#   )
# our_implementation_probit
# # comparing results to glm output
# r_implementation_probit <- summary(glm(y ~ x1 + x2,
#                                        data = test_probit_regression_data,
#                                        family = binomial(link = 'probit')))
# r_implementation_probit

################# results are similar

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
    print(implementation_probit, estimate)
    pred <- ifelse(X %*% estimate < 0, 0, 1)
    return(pred)
  }

x1 = rnorm(1000, 0, 1)
x2 = rnorm(1000, 0, 2)
test_probit_regression_data <- data.frame(x1)
test_probit_regression_data$y <-
  rbinom(n = 1000, size = 1, prob = plogis(x2))
plot(test_probit_regression_data$y)
imp <- probit_regression(
  test_probit_regression_data,
  test_probit_regression_data$x1,
  test_probit_regression_data$x2,
  y = test_probit_regression_data$y
)
summary(glm(y ~ x1 + x2, data = test_probit_regression_data,
            family = binomial(link = 'probit')))
predict_probit(
  test_probit_regression_data,
  test_probit_regression_data$x1,
  test_probit_regression_data$x2,
  y = test_probit_regression_data$y,
  implementation_probit = imp
)
predicting_accuracy_all_assumptions_met <- sum(
  predict_probit(
    test_probit_regression_data,
    test_probit_regression_data$x1,
    test_probit_regression_data$x2,
    y = test_probit_regression_data$y,
    implementation_probit = imp
  )[, 1] == test_probit_regression_data$y
)
predicting_accuracy_all_assumptions_met # high accuracy







# predict_probit(
#   test_probit_regression_data,
#   test_probit_regression_data$x1,
#   test_probit_regression_data$x2,
#   y = test_probit_regression_data$y,
#   implementation_probit = our_implementation_probit
# )
# predicting_accuracy_all_assumptions_met <- sum(
#   predict_probit(
#     test_probit_regression_data,
#     test_probit_regression_data$x1,
#     test_probit_regression_data$x2,
#     y = test_probit_regression_data$y,
#     implementation_probit = our_implementation_probit
#   )[, 1] == test_probit_regression_data$y
# )
# predicting_accuracy_all_assumptions_met # high accuracy

# assumption 1: the outcome is binary
# breaking this assumption -- the implementation of this fails
# test_probit_regression_data_outcome_not_binary <-
#   data.frame(x1 = rnorm(100, 5, 2),
#              x2 = rnorm(100, 0, 2))
# test_probit_regression_data_outcome_not_binary$y <-
#   rnorm(100)
# our_implementation_probit_outcome_not_binary <- probit_regression(
#   test_probit_regression_data_outcome_not_binary,
#   test_probit_regression_data_outcome_not_binary$x1,
#   test_probit_regression_data_outcome_not_binary$x2,
#   y = test_probit_regression_data_outcome_not_binary$y
# )
# predict_probit(
#   test_probit_regression_data_outcome_not_binary,
#   test_probit_regression_data_outcome_not_binary$x1,
#   test_probit_regression_data_outcome_not_binary$x2,
#   y = test_probit_regression_data_outcome_not_binary$y,
#   implementation_probit = our_implementation_probit_outcome_not_binary
# )

# assumption 2: the z score of the outcome and the predictors have a linear relationship
# breaking this assumption
x1 <- rnorm(1000, 0.0, 0.2)
x2 = rnorm(1000, 0.0, 0.2)
test_probit_regression_data_not_linear <-
  data.frame(x1,
             x2,
             y_prob = x1 * x2 - x1 + x2)

# test_probit_regression_data_not_linear$y_prob <-
#   plogis(test_probit_regression_data_not_linear$x2)

test_probit_regression_data_not_linear$y <-
  ifelse(test_probit_regression_data_not_linear$y_prob < 0.0, 0, 1)
our_implementation_probit_not_linear <-
  probit_regression(
    test_probit_regression_data_not_linear,
    test_probit_regression_data_not_linear$x1,
    test_probit_regression_data_not_linear$x2,
    y = test_probit_regression_data_not_linear$y
  )
summary(glm(y ~ x1 + x2, data = test_probit_regression_data_not_linear,
            family = binomial(link = 'probit')))


predict_probit(
  test_probit_regression_data_not_linear,
  test_probit_regression_data_not_linear$x1,
  test_probit_regression_data_not_linear$x2,
  y = test_probit_regression_data_not_linear$y,
  implementation_probit = our_implementation_probit_not_linear
)
predicting_accuracy_not_linear <- sum(
  predict_probit(
    test_probit_regression_data_not_linear,
    ... = c(
      test_probit_regression_data_not_linear$x1,
      test_probit_regression_data_not_linear$x2
    ),
    y = test_probit_regression_data_not_linear$y,
    implementation_probit = our_implementation_probit_not_linear
  )[, 1] == test_probit_regression_data_not_linear$y
)

as.numeric(t(test_probit_regression_data_not_linear$y)) == as.numeric(
  predict_probit(
    test_probit_regression_data_not_linear,
    test_probit_regression_data_not_linear$x1,
    test_probit_regression_data_not_linear$x2,
    y = test_probit_regression_data_not_linear$y,
    implementation_probit = our_implementation_probit_not_linear
  )
)



# assumption 3: errors are are normally distributed
# (and are independent but we will not be able to show this using code)
# breaking this assumption

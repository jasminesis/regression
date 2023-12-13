################# linear regression function
linear_regression <- function(data, ..., y) {
  x_parameters <- c(...)
  n <- nrow(data)
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  Y <- matrix(y, nrow = n, ncol = 1)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  estimate <- c()
  for (i in 1:ncol(X)) {
    estimate[i] <- beta[i]
  }
  num_bootstraps <- 10000
  bootstrap_betas <-
    matrix(0, nrow = num_bootstraps, ncol = ncol(data))
  for (i in 1:num_bootstraps) {
    sample_indices <- sample(nrow(data), replace = TRUE)
    bootstrap_data <- data[sample_indices,]
    bootstrap_X <-
      as.matrix(cbind(1, bootstrap_data[, 1:(ncol(bootstrap_data) - 1)]))
    bootstrap_Y <- as.matrix(bootstrap_data$y, ncol = 1)
    bootstrap_beta <-
      solve(t(bootstrap_X) %*% bootstrap_X) %*% t(bootstrap_X) %*% bootstrap_Y
    bootstrap_betas[i,] <- bootstrap_beta
  }
  se <- c()
  for (i in 1:ncol(X)) {
    se[i] <- apply(bootstrap_betas, 2, sd)[i]
  }
  rownames <- c()
  for (i in 1:((ncol(X)) - 1)) {
    rownames[i] <-  i
  }
  t <- estimate / se
  df <- nrow(X) - ncol(X)
  p <- 2 * pt(t, df, lower = F)
  residual <- sqrt(mean((Y - X %*% beta) ^ 2))
  return(
    data.frame(
      Estimate = estimate,
      Std.Error = se,
      t.value = t,
      p.value = p,
      Residual = c(residual, rep(NA, ncol(X) - 1)),
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

# assumption 1: x and y follow a linear relationship
# following this assumption
# say, we follow this assumption by considering the
# data above, test_linear_regression_data. The residual
# for the regression on this data set is:
our_implementation$Residual[1] # a small residual here
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


# breaking this assumption:
x <- rnorm(100, mean = 5, sd = 2)
y <- x ^ 4 + rnorm(100, mean = 0, sd = 1)
plot(x, y, main = 'x and y clearly do not have a linear relationship')
breaking_assumption_1_data <- data.frame(x = x, y = y)
linear_regression(data = breaking_assumption_1_data,
                  breaking_assumption_1_data$x,
                  y = breaking_assumption_1_data$y)
intercept <-
  as.numeric(sub(
    ".*:\\s*",
    "",
    linear_regression(
      data = breaking_assumption_1_data,
      breaking_assumption_1_data$x,
      y = breaking_assumption_1_data$y
    )[1]
  ))
slope <-
  as.numeric(sub(
    ".*:\\s*",
    "",
    linear_regression(
      data = breaking_assumption_1_data,
      breaking_assumption_1_data$x,
      y = breaking_assumption_1_data$y
    )[2]
  ))
abline(a = intercept, b = slope)
sq_error_terms <-
  ((intercept + slope * breaking_assumption_1_data$x) - breaking_assumption_1_data$y) ^
  2

plot(sq_error_terms)
abline(h = mean(sq_error_terms))
# should we break assumptions by showing how far off the prediction is from
# actual values and compare to when the assumptions are met?




residual_comparison <- data.frame(resid_all_assumptions_met = our_implementation$Residual[1],
                                  resid_not_linear = our_implementation_not_linear$Residual[1],
                                  resid_not_normally_dist = our_implementation_not_normally_dist$Residual[1])

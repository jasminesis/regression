################# linear regression function
linear_regression <- function(data, x1, x2, ..., y) {
  n <- nrow(data)
  x_parameters <- c(x1, x2, ...)
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  Y <- matrix(y, nrow = n, ncol = 1)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  paste0(
    'the intercept of the regression is ',
    beta[1],
    ' the coefficient of x1 is ',
    beta[2],
    ' the coefficient of x2 is ',
    beta[3]
  )
}
# test data frame for linear regression
test_linear_regression_data <- data.frame(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(1, 4, 5, 7, 9),
  x3 = c(43, 67, 45, 12, 0),
  y = rnorm(5, mean = 0, sd = 0.3)
)
# applying created function to test data frame
linear_regression(
  test_linear_regression_data,
  x1 = test_linear_regression_data$x1,
  x2 = test_linear_regression_data$x2,
  x3 = test_linear_regression_data$x3,
  y = test_linear_regression_data$y
)
# comparing results to lm output
lm(y ~ x1 + x2 + x3, data = test_linear_regression_data)

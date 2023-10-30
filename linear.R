################# linear regression function
linear_regression <- function(data, ..., y) {
  # we assume that the number of observations correspond to what the user wants to use in the regression
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
    estimate[i] <- paste0('The coefficient of beta', i - 1,
                          ' is: ', beta[i])
  }
  num_bootstraps <- 100
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
    se[i] <- paste0('The bootstrapped standard error of beta', i - 1,
                          ' is: ', apply(bootstrap_betas, 2, sd)[i])
  }
  return(list(estimate, se))
}
# test data frame for linear regression
test_linear_regression_data <- data.frame(
  x1 = c(1, 2, 3, 4, 5),
  x2 = rnorm(100, mean = 0, sd = 2),
  x3 = rnorm(100, mean = 4, sd = 6),
  x4 = runif(100, min = 4, max = 5),
  y = rnorm(100, mean = 0, sd = 0.3)
)
# applying created function to test data frame
linear_regression(
  test_linear_regression_data,
  test_linear_regression_data$x1,
  test_linear_regression_data$x2,
  test_linear_regression_data$x3,
  test_linear_regression_data$x4,
  y = test_linear_regression_data$y
)
# comparing results to lm output
summary(lm(y ~ x1 + x2 + x3 + x4, data = test_linear_regression_data))




# assumption 1: x and y follow a linear relationship
# breaking this assumption:
x <- rnorm(100, mean = 5, sd = 2)
y <- x ^ 4 + rnorm(100, mean = 0, sd = 1)
plot(x, y, main = 'x and y clearly do not have a linear relationship')
breaking_assumption_1_data <- data.frame(x = x, y = y)
linear_regression(data = breaking_assumption_1_data, 
                  breaking_assumption_1_data$x, 
                  y = breaking_assumption_1_data$y)
intercept <- as.numeric(sub(".*:\\s*", "", linear_regression(data = breaking_assumption_1_data, 
                               breaking_assumption_1_data$x, 
                               y = breaking_assumption_1_data$y)[1]))
slope <- as.numeric(sub(".*:\\s*", "", linear_regression(data = breaking_assumption_1_data, 
                           breaking_assumption_1_data$x, 
                           y = breaking_assumption_1_data$y)[2]))
abline(a = intercept, b = slope)
sq_error_terms <- ((intercept + slope*breaking_assumption_1_data$x) - breaking_assumption_1_data$y)^2
  
plot(sq_error_terms)
abline(h = mean(sq_error_terms))
# should we break assumptions by showing how far off the prediction is from 
# actual values and compare to when the assumptions are met? 

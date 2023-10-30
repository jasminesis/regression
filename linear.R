################# linear regression function
linear_regression <- function(data, ..., y) {
  # we assume that the number of observations correspond to what the user wants to use in the regression
  n <- nrow(data)
  x_parameters <- c(...)
  predictors <- c()
  for (i in list(x_parameters)){
    predictors <- as.vector(data$`i`)
  }
  X <-
    matrix(c(rep(1, n), x_parameters),
           nrow = n,
           ncol = ncol(data))
  Y <- matrix(y, nrow = n, ncol = 1)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  output <- c()
  for (i in 1:ncol(X)) {
    output[i] <- paste0('The coefficient of beta', i - 1,
                        ' is: ', beta[i])
  }
  return(output)
}
# test data frame for linear regression
test_linear_regression_data <- data.frame(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(1, 4, 5, 7, 9),
  x3 = c(43, 67, 45, 12, 0),
  y = rnorm(5, mean = 0, sd = 0.3)
)
# applying created function to test data frame
linear_regression(test_linear_regression_data, 
                  test_linear_regression_data$x1, 
                  test_linear_regression_data$x2, 
                  test_linear_regression_data$x3, 
                  y = test_linear_regression_data$y)
# comparing results to lm output
lm(y ~ x1 + x2 + x3, data = test_linear_regression_data)

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

library(ggplot2)
library(MASS)
library(glmbb) # for crabs data

poisson_function <- function(fn_formula, data) {
  number_omitted <- nrow(data) - nrow(na.omit(data))
  data <- na.omit(data)

  vars <- all.vars(as.formula(fn_formula))
  y_name <- vars[1]
  x_name <- vars[2:length(vars)]
  n <- nrow(data)
  Y <- matrix(data[, y_name], nrow = n, ncol = 1)
  X <- matrix(cbind(rep(1, n)))

  # take in categorical data
  var_names <- vector("character")
  for (i in x_name) {
    if (suppressWarnings(all(!is.na(as.numeric(as.character(data[, i])))))) {
      X <- cbind(X, as.numeric(as.character(data[, i])))
      var_names <- c(var_names, i)
    } else {
      categories <- sort(unique(data[, i]))
      for (j in categories[2:length(categories)]) {
        new_col_name <- paste0(i, j)
        new_col <- ifelse(data[, i] == j, 1, 0)
        X <- cbind(X, new_col)
        var_names <- c(var_names, new_col_name)
      }
    }
  }
  optim_poisson <- function(beta, X, Y) {
    beta <- as.matrix(beta, nrow = 4)
    beta_x <- X %*% beta
    loglikelihood <- -sum(Y * beta_x - exp(beta_x))
    return(loglikelihood)
  }
  result <- optim(par = rep(0, ncol(X)), fn = optim_poisson, X = X, Y = Y, hessian = T)
  OI <- solve(result$hessian)
  se <- sqrt(diag(OI))
  z_value <- result$par / se
  df <- nrow(X) - ncol(X)
  p_value <- 2 * pnorm(-1 * abs(z_value))
  # https://stats.stackexchange.com/questions/52475/how-are-the-p-values-of-the-glm-in-r-calculated

  coef <- rbind(result$par, se, z_value, p_value)

  colnames(coef) <- c("(Intercept)", var_names)
  rownames(coef) <- c("Estimate", "Std. Error", "z value", "p value")
  return(t(coef))
}

# very important in Poisson Regression is equidispersion, which means that the mean and variance of the distribution are equal.
set.seed(2012)
n <- 1000
x1 <- runif(n, 0, 100)
results <- matrix(NA, ncol = 2, nrow = 1e4)
lambda <- exp(1 + 0.3 * x1 + rnorm(n))
y <- rpois(n, lambda = lambda)
# super over-dispersed
mean(y)
var(y)
sim_data <- data.frame(y, x1)
summary(glm(y ~ x1, family = poisson, data = sim_data))$coef
poisson_function(fn_formula = "y ~ x1", data = sim_data)


# comparing coefficients with crabs data
data(crabs, package="glmbb")
summary(glm(satell ~ width, family = poisson(link = "log"), data = crabs))$coef

# a bit over-dispersed
mean(crabs$satell)
var(crabs$satell)

poisson_function(fn_formula = "satell ~ width", data = crabs)
ggplot(crabs) +
  geom_histogram(aes(x = satell))
  # geom_histogram(aes(x = width, y = satell))

M1 <- glm(satell ~ width, family = poisson(link = "log"), data = crabs)
M2 <- glm.nb(satell ~ width, data = crabs)
M3 <- pscl::zeroinfl(satell ~ width | width, data=crabs)

# estimate overdispersion
estimate_overdisp <- function(model_obj, data) {
  z <- resid(model_obj, type = "pearson")
  n <- nrow(data)
  k <- length(coef(model_obj))
  overdisp_ratio <-  sum(z^2) / (n - k)
  p_val <- pchisq(sum(z^2), (n - k))
  return(c(overdisp_ratio, p_val))
}
estimate_overdisp(M1, crabs)
estimate_overdisp(M2, crabs)
estimate_overdisp(M3, crabs)

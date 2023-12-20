library(pscl)

zippoisson_function <- function(fn_formula, data) {
  number_omitted <- nrow(data) - nrow(na.omit(data))
  data <- na.omit(data)

  vars <- all.vars(as.formula(fn_formula))
  y_name <- vars[1]
  covL <- data[, vars[2]]
  covp <- data[, vars[3]]
  n <- nrow(data)
  Y <- matrix(data[, y_name], nrow = n, ncol = 1)

  optim_zip <- function(beta) {
    lambda <- exp(beta[1] + beta[2] * covL)
    p <- plogis(beta[3] + beta[4] * covp)
    lik <- p * (Y == 0) + (1 - p) * dpois(Y, lambda)
    return(-sum(log(lik)))
  }
  result <- optim(par = rep(0, 4), fn = optim_zip, hessian = T)
  OI <- solve(result$hessian)
  se <- sqrt(diag(OI))
  z_value <- result$par / se
  p_value <- 2 * pnorm(-1 * abs(z_value))

  coef <- rbind(result$par, se, z_value, p_value)

  colnames(coef) <- c("(Intercept)", vars[2], "(Intercept)", vars[3])
  rownames(coef) <- c("Estimate", "Std. Error", "z value", "p value")
  return(t(coef))
}

set.seed(1)
n <- 1000
covL <- seq(0, 1, length.out = n)
covp <- seq(0, 1, length.out = n)
trueMeans <- exp(1.5 - 0.5 * covL)
probability <- plogis(-0.5 + 2.5 * covp)
U <- runif(n, 0, 1)
y <- rpois(n, trueMeans)
y[U < probability] <- 0
sim_data <- data.frame(y, covL, covp)
hist(sim_data$y, main="Histogram of Zero-inflated Poisson", xlab="Count")

# comparing performance of our implementation with zeroinfl
zippoisson_function(fn_formula = "y ~ covL | covp", data = sim_data)
summary(pscl::zeroinfl(y ~ covL | covp))

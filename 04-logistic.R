library(alr4)
set.seed(1)
# invlogit <- plogis

logistic_function <- function(fn_formula, data, predict = F) {
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
  optim_logistic <- function(beta, X, Y) {
    beta <- as.matrix(beta, nrow = 4)
    pi <- plogis(X %*% beta)
    loglikelihood <- -sum(Y * log(pi) + (1 - Y) * log(1 - pi))
    return(loglikelihood)
  }
  result <- optim(par = rep(0, ncol(X)), fn = optim_logistic, X = X, Y = Y, hessian = T)
  OI <- solve(result$hessian)
  se <- sqrt(diag(OI))
  t_statistic <- result$par / se
  df <- nrow(X) - ncol(X)
  p_value <- 2 * pnorm(-1 * abs(t_statistic))
  # https://stats.stackexchange.com/questions/52475/how-are-the-p-values-of-the-glm-in-r-calculated

  coef <- rbind(result$par, se, t_statistic, p_value)
  colnames(coef) <- c("(Intercept)", var_names)
  rownames(coef) <- c("Estimate", "Std. Error", "z value", "p value")
  coef <- t(coef)

  b_hat <- result$par
  predictions <- plogis(X %*% b_hat)

  if (predict) {
    return(predictions)
  } else {
    return(coef)
  }
}
head(predict(fit_sim_data, type="response"))
# create fake data
sim_data <- data.frame(x1 = rnorm(100, 2, 1), x2 = rnorm(100, 4, 1), x3 = rnorm(100, 6, 1))
sim_data$y <- rbinom(100,
  size = 1, prob =
    plogis(-1 + sim_data$x1 + sim_data$x2 - 0.5 * sim_data$x3)
)

# compare DIY logistic function with glm
fit_sim_data <- glm(y ~ x1 + x2 + x3, data = sim_data, family = binomial)
summary(fit_sim_data)$coef
logistic_function(fn_formula = "y ~ x1 + x2 + x3", data = sim_data)

# compare DIY logistic function with glm for categorical data
Donner$survived <- Donner$y == "survived"
fit_Donner <- glm(survived ~ age + sex + status, data = Donner, family = "binomial")
summary(fit_Donner)$coef
logistic_function(fn_formula = "survived ~ age + sex + status", data = Donner)

# get_prediction
logistic_function(fn_formula = "survived ~ age + sex + status", data = Donner, predict=T)

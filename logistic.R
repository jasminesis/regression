library(tidyverse)
library(alr4)
set.seed(1)
# invlogit <- plogis

logistic_function <- function(fn_formula, data) {
  number_omitted <- nrow(data) - nrow(na.omit(data))
  data <- na.omit(data)

  vars <- all.vars(as.formula(fn_formula))
  y_name <- vars[1]
  x_name <- vars[2:length(vars)]
  n <- nrow(data)
  Y <- matrix(data[, y_name], nrow = n, ncol = 1)
  X <- matrix(cbind(rep(1, n)))

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
  result <- optim(par = rep(0, ncol(X)), fn = optim_logistic, X = X, Y = Y)
  coef <- t(as.matrix(result$par))
  colnames(coef) <- c("(Intercept)", var_names)
  return(coef)
}

sim_data <- data.frame(x1 = rnorm(100, 2, 1), x2 = rnorm(100, 4, 1), x3 = rnorm(100, 6, 1))
sim_data$y <- rbinom(100, size = 1, prob = plogis(-1 + sim_data$x1 + sim_data$x2 - 0.5 * sim_data$x3))

fit_sim_data <- glm(y ~ x1 + x2 + x3, data = sim_data, family = binomial)
coef(fit_sim_data)
logistic_function(fn_formula = "y ~ x1 + x2 + x3", data = sim_data)


Donner <- Donner %>% mutate(survived = y == "survived")
fit_Donner <- glm(survived ~ age + sex + status, data = Donner, family = "binomial")
coef(fit_Donner)
logistic_function(fn_formula = "survived ~ age + sex + status", data = Donner)

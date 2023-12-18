zipoisson_function <- function(fn_formula, data) {
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
  optim_zipoisson <- function(beta, X, Y) {
    beta <- as.matrix(beta, nrow = 4)
    beta_x <- X %*% beta
    # loglikelihood <- -sum(Y * beta_x - exp(beta_x))
    return(loglikelihood)
  }
  result <- optim(par = rep(0, ncol(X)), fn = optim_zipoisson, X = X, Y = Y, hessian = T)
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

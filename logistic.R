require(tidyverse)

logit <- qlogis
invlogit <- plogis

dgp <- function() {
  n <- 200
  x <- sample(c(seq(from = 0, to = 6, by = 0.05)), n, replace = T)
  y <- rbinom(n, 1, x / 6)
  tibble(y, x) %>% arrange(x)
}
data <- dgp()

fit <- glm(y ~ x, family = binomial(link = "logit"), data = data)
fit
ggplot(data) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "glm", method.args = list(family = "binomial"), se = F)

# taken from https://francish.net/post/logistic-regression-fisher-scoring/

### extracting raw components

dat <- model.frame(fit)
fml <- formula(fit)
X <- model.matrix(fml, dat)
y <- model.response(dat, "numeric")
k <- ncol(X)
# pp <- function(x) 1 / (1 + exp(-x)) #convert logit to pred prob

beta_0 <- rep(0, k) # initialize with zeroes
eta <- X %*% beta_0 # predicted logit
mu <- plogis(eta) # same as pp but built-in; convert logit to pred prob
vr <- (mu * (1 - mu)) # variance

wts <- diag(as.vector(vr)) # create weight matrix
d2 <- t(X) %*% wts %*% X # the Fisher information matrix
u1 <- t(X) %*% (y - mu) # the score function
iter <- 50 # max number of iterations
tol <- 1e-8 # tolerance

### now iterate

for (i in 1:iter) {
  # beta_1 <- beta_0 + solve(d2) %*% u1 #update beta
  beta_1 <- beta_0 + chol2inv(chol(d2)) %*% u1 # update beta avoiding inversion
  print(beta_1) #show estimates

  if (any(abs((beta_1 - beta_0) / beta_0) < tol)) break # stop loop if no change

  eta <- X %*% beta_1 # predicted logit
  mu <- plogis(eta) # this is just pp(eta)
  vr <- mu * (1 - mu) # variance

  wts <- diag(as.vector(vr)) # putting in a weight matrix
  d2 <- t(X) %*% wts %*% X # information matrix
  u1 <- t(X) %*% (y - mu) # score function
  beta_0 <- beta_1
  cat(i, "::") # show iteration number
}




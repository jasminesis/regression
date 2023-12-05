library(tidyverse)
library(alr4)
set.seed(1)

invlogit <- plogis
optim_logistic <- function(beta) {
  alpha <- beta[1]
  b1 <- beta[2]
  b2 <- beta[3]
  b3 <- beta[4]
  pi <- invlogit(alpha + b1 * data$x1 + b2 * data$x2 + b3 * data$x3)
  loglikelihood <- -sum(data$y * log(pi) + (1 - data$y) * log(1 - pi))
  return(loglikelihood)
}
data <- data.frame(x1 = rnorm(100, 2, 1), x2 = rnorm(100, 4, 1), x3 = rnorm(100, 6, 1))
data$y <- rbinom(100, size = 1, prob = invlogit(-1 + data$x1 + data$x2 - 0.5 * data$x3))

result <- optim(par = rep(0, ncol(data)), fn = optim_logistic)
result$par

glm(y ~ x1 + x2 + x3, data = data, family = binomial)


# extend the function to be able to take in categorical data
Donner <- Donner %>% mutate(survived = y == "survived")
m1 <- glm(survived ~ age + sex + status, family = binomial(link = "logit"), data = Donner)
ggplot(Donner) +
  geom_jitter(aes(x = status, y = age, col = survived))

ggplot(Donner) +
  geom_jitter(aes(x = age, y = survived, color = survived)) +
  facet_wrap(vars(status))
ggplot(Donner) +
  geom_jitter(aes(x = age, y = status, color = status)) +
  facet_wrap(vars(survived))

fit <- glm(survived ~ age + sex + status, data = Donner, family = "binomial")
coef(fit)

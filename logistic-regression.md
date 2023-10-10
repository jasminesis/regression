# Logistic Regression

Gradient descent

Fisher’s Scoring algorithm

- efficiently solves for $\beta$

**Notes from George:**

I was thinking more about your project and the Maximum Likelihood for logistic and poisson regression and wanted to share some additional thoughts.

There are a few different ways you can estimate the parameters, some of the most popular are iterative reweighted least squares (this is what R does), gradient descent methods or Newton-Ralphson.

If you use gradient descent you can use the optim function in base R, you dont have to write your own optimization algorithm. This will give you an MLE estimate without having to do a full grid search.

The iterative reweighted least squares and Newton-Ralphson approach is the one that uses matrix multiplication but it is more complex than the simple matrix equation for OLS and will involve some sort of a while loop.

For this project you can use any of the tools available (gradient descent, NR or IWLS)

Assume you can use any function in base R outside of the direct regression function.

optim - for gradient descent

ralphson - big while loop

giving … arg for R lets you take in anything

- formula class in R - recreate a formula call
    - as.formula() - to change it to something that can be taken in
- probably want to change the input for regression to a matrix of x (predictors) and a y (outcome)
    - matrix is good cos all will be numbers
    - but if dataframe with factors/levels → need to be able to redo the factors/levels to numbers

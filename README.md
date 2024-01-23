# Regression Implementations in R

[See the implementations in R here!](https://diy-regression-stats.netlify.app/)

For NYU Stats Consulting Fall 2023 (taught by [George Perrett](https://github.com/gperrett/))

By Jasmine Siswandjo and Saumya Seth

1. Linear regression - Saumya
2. Probit regression - Saumya
3. Negative Binomial regression - Saumya
4. Logistic regression - Jasmine
5. Poisson regression - Jasmine
6. Zero-inflated Poisson regression - Jasmine

## Goals of the Project:

- To implement our own functions for regression without using in-built regression packages in R. We will estimate coefficients of the predictors, estimate their standard errors, and calculate the p-values of said models
- To highlight and check the assumptions of each model
- To discuss the implications of breaking assumptions for the models
- To discuss the applications of the models and compare and contrast situations where certain models may perform better than others

## Instructions on publishing to website

There are two branches in this repo: `main` and `website`. They are completely different, so never merge the two! `website` contains a Quarto website project, and should be published by following these steps.

1. `git checkout website`
2. `quarto netlify publish`

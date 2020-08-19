
<!-- README.md is generated from README.Rmd. Please edit that file -->

FIHR
====

<!-- badges: start -->
<!-- badges: end -->

The goal of FIHR is to provide inference for linear and quadratic
functionals in high-dimensional linear and logistic regression models.
It computes bias-corrected estimators and corresponding standard errors
for the linear and quadratic functionals.

Installation
------------

You can install the released version of FIHR from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("FIHR")

And the development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("prabrishar1/FIHR")

Example
-------

These are basic examples which show how to solve the common
high-dimensional inference problems:

    library(FIHR)

Inference for linear functional in high-dimensional linear regression
model


    library(MASS)
    A1gen <- function(rho,p){
      A1=matrix(0,p,p)
      for(i in 1:p){
        for(j in 1:p){
          A1[i,j]<-rho^(abs(i-j))
        } 
      }
      A1
    }
    mu <- rep(0,400)
    mu[1:5] <- c(1:5)/5
    rho = 0.5
    Cov <- (A1gen(rho,400))/2
    beta <- rep(0,400)
    beta[1:10] <- c(1:10)/5
    X <- MASS::mvrnorm(100,mu,Cov)
    y = X%*%beta + rnorm(100)
    Est = FIHR::LF(X = X, y = y, loading = c(1,rep(0,399)), intercept = TRUE)
    #> [1] 3
    #> [1] "step is 3"
    Est$prop.est
    #>            [,1]
    #> [1,] 0.02886183
    Est$se
    #> [1] 0.1182428

Inference for linear functional in high-dimensional logistic regression
model

    library(MASS)
    A1gen <- function(rho,p){
      A1=matrix(0,p,p)
      for(i in 1:p){
        for(j in 1:p){
          A1[i,j]<-rho^(abs(i-j))
        } 
      }
      A1
    }
    mu <- rep(0,400)
    rho = 0.5
    Cov <- (A1gen(rho,400))/2
    beta <- rep(0,400)
    beta[1:10] <- c(1:10)/5
    X <- MASS::mvrnorm(100,mu,Cov)
    exp_val <- X%*%beta
    prob <- exp(exp_val)/(1+exp(exp_val))
    y <- rbinom(100,1,prob)
    Est = FIHR::LF_logistic(X = X, y = y, loading = c(1,rep(0,399)), intercept = TRUE, weight = rep(1,100))
    #> Warning in htheta * col.norm: longer object length is not a multiple of shorter
    #> object length
    #> [1] 3
    #> [1] "step is 3"
    Est$prop.est
    #> [1] -0.02808497
    Est$se
    #> [1] 0.2971103

Inference for quadratic functional in high-dimensional linear model

    library(MASS)
    A1gen <- function(rho,p){
      A1=matrix(0,p,p)
      for(i in 1:p){
        for(j in 1:p){
          A1[i,j]<-rho^(abs(i-j))
        } 
      }
      A1
    }
    rho = 0.6
    Cov <- (A1gen(rho,400))
    mu <- rep(0,400)
    mu[1:5] <- c(1:5)/5
    beta <- rep(0,400)
    beta[25:50] <- 0.08
    X <- MASS::mvrnorm(100,mu,Cov)
    y <- X%*%beta + rnorm(100)
    test.set <- c(30:100)

    ## Inference for Quadratic Functional with Population Covariance Matrix in middle

    Est = FIHR::QF(X = X, y = y, test.set=test.set)
    Est$prop.est
    #>           [,1]
    #> [1,] 0.3471611
    Est$se
    #> [1] 0.1079703

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = FIHR::QF(X = X, y = y, test.set=test.set, A = diag(1:400,400))
    #> Warning in if (A != "Sigma") {: the condition has length > 1 and only the first
    #> element will be used
    #> Warning in if (A == "Sigma") {: the condition has length > 1 and only the first
    #> element will be used
    #> [1] "A is not Sigma 2"
    #> Warning in if (A == "Sigma") {: the condition has length > 1 and only the first
    #> element will be used
    #> [1] "A is not Sigma 3"
    Est$prop.est
    #>          [,1]
    #> [1,] 50.81775
    Est$se
    #> [1] 0.1435641

    ## Inference for square norm of regression vector

    Est = FIHR::QF(X = X, y = y, test.set=test.set, A = diag(1,400))
    #> Warning in if (A != "Sigma") {: the condition has length > 1 and only the first
    #> element will be used

    #> Warning in if (A != "Sigma") {: the condition has length > 1 and only the first
    #> element will be used
    #> [1] "A is not Sigma 2"
    #> Warning in if (A == "Sigma") {: the condition has length > 1 and only the first
    #> element will be used
    #> [1] "A is not Sigma 3"
    Est$prop.est
    #>           [,1]
    #> [1,] 0.8310707
    Est$se
    #> [1] 0.1414572

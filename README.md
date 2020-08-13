
<!-- README.md is generated from README.Rmd. Please edit that file -->
FIHR
====

<!-- badges: start -->
<!-- badges: end -->
The goal of FIHR is to provide inference for linear and quadratic functionals in high-dimensional linear and logistic regression models. It computes bias-corrected estimators and corresponding standard errors for the linear and quadratic functionals.

Installation
------------

You can install the released version of FIHR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("FIHR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("prabrishar1/FIHR")
```

Example
-------

These are basic examples which show how to solve the common high-dimensional inference problems:

``` r
library(FIHR)
```

Inference for linear functional in high-dimensional linear regression model

``` r

#X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
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
Est = FIHR::LF_Inference(X = X, y = y, loading = c(1,rep(0,399)), intercept = TRUE)
#> [1] 1
#> [1] 0.1578344
#> [1] 2
#> [1] 0.2288057
#> [1] 3
#> [1] 0.30179
#> [1] 4
#> [1] 3
#> [1] "step is 3"
Est$prop.est
#>         [,1]
#> [1,] 0.23689
Est$se
#> [1] 0.1646678
```

Inference for linear functional in high-dimensional logistic regression model

``` r
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
#Est = FIHR::LF_Inference_logistic(X = matrix(sample(-2:2,50*300,replace = TRUE),nrow=50,ncol=300),
#                      y = rbinom(50,1,0.5), loading = c(1,rep(0,299)),
#                       intercept = TRUE, weight = rep(1,50))
Est = FIHR::LF_Inference_logistic(X = X, y = y, loading = c(1,rep(0,399)), intercept = TRUE, weight = rep(1,100))
#> [1] 3
#> [1] "step is 3"
Est$prop.est
#> [1] -0.2664569
Est$se
#> [1] 0.5373868
```

Quadratic Functional (Group\_Covariance) Linear Model (will be removed)

``` r
X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
beta = (1:400)/25
y = X%*%beta + rnorm(100,0,1)
Est = FIHR::QF_Cov(X = X, y = y, test.set=c(30:50))
Est$prop.est
#>          [,1]
#> [1,] 928.5145
Est$se
#> [1] 19538.58
```

Inference for sqaure norm of the regression vector in high-dimensional linear model

``` r

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
#X = MASS::mvrnorm(n = 100, mu = rep(0,400), Sigma = diag(1,400))
#X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#beta = (1:400)/25
#y = X%*%beta + rnorm(100,0,1)
Est = FIHR::QF_Norm(X = X, y = y, test.set=test.set)
#> [1] 1
#> [1] 0.02683212
#> [1] 2
#> [1] 0.03950055
#> [1] 3
#> [1] 0.05150157
#> [1] 4
#> [1] 3
#> [1] "step is 3"
Est$prop.est
#>           [,1]
#> [1,] 0.2686378
Est$se
#> [1] 0.1466222
```

Inference for quadratic functional in high-dimensional linear model

``` r
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
#X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#beta = (1:400)/25
#y = X%*%beta + rnorm(100,0,1)
#Est = FIHR::Group_Test(X = X, y = y, test.set=c(30:50))
Est = FIHR::QF(X = X, y = y, test.set=test.set)
#> Warning in FIHR::QF(X = X, y = y, test.set = test.set): The model is most likely
#> misspecified because the correction term is larger than the lasso estimate
#> in absolute value. See cluster or group: . The value of the lasso.plugin and
#> correction are 0.01916 respectively 0.02101 .
Est$prop.est
#>            [,1]
#> [1,] 0.04017795
Est$se
#> [1] 0.1002837
```

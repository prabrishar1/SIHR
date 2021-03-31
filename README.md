
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1)linear functionals (LF) and quadratic
functionals (QF) in linear regression, (2)linear functional in logistic
regression, (3) individual treatment effects (ITE) in linear and
logistic regression.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("prabrishar1/SIHR")

## Example

These are basic examples which show how to solve the common
high-dimensional inference problems:

    library(SIHR)

Inference for linear functional in high-dimensional linear regression
model


    library(MASS)
    n = 100
    p = 400
    A1gen <- function(rho,p){
      A1=matrix(0,p,p)
      for(i in 1:p){
        for(j in 1:p){
          A1[i,j]<-rho^(abs(i-j))
        } 
      }
      A1
    }
    mu <- rep(0,p)
    mu[1:5] <- c(1:5)/5
    rho = 0.5
    Cov <- (A1gen(rho,p))/2
    beta <- rep(0,p)
    beta[1:10] <- c(1:10)/5
    X <- MASS::mvrnorm(n,mu,Cov)
    y = X%*%beta + rnorm(n)
    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est = SIHR::LF(X = X, y = y, loading = loading, intercept = TRUE)
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] -3.782195

    ### Standard error 

    Est$se
    #> [1] 2.055267

    ### Confidence interval
    Est$CI
    #> [1] -7.8104444  0.2460535

    ### test whether the linear functional is below zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 0

Individualised Treatment Effect in high-dimensional logistic regression
model


    n1 = 100
    p = 400
    n2 = 100
    A1gen <- function(rho,p){
    A1=matrix(0,p,p)
    for(i in 1:p){
    for(j in 1:p){
      A1[i,j]<-rho^(abs(i-j))
    }
    }
    A1
    }
    mu <- rep(0,p)
    rho = 0.5
    Cov <- (A1gen(rho,p))/2
    beta1 <- rep(0,p)
    beta1[1:10] <- c(1:10)/5
    beta2 <- rep(0,p)
    beta2[1:5] <- c(1:5)/10
    X1 <- MASS::mvrnorm(n1,mu,Cov)
    X2 <- MASS::mvrnorm(n2,mu,Cov)
    y1 = X1%*%beta1 + rnorm(n1)
    y2 = X2%*%beta2 + rnorm(n2)
    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est <- SIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
    #> [1] "step is 3"
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>            [,1]
    #> [1,] -0.1067286

    ### Standard error 

    Est$se
    #> [1] 2.909604

    ### Confidence interval
    Est$CI
    #> [1] -5.809448  5.595991

    ### test whether the linear ITE is below zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 0

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
    n = 100
    p = 400
    mu <- rep(0,p)
    rho = 0.5
    Cov <- (A1gen(rho,p))/2
    beta <- rep(0,p)
    beta[1:10] <-0.5*c(1:10)/10
    X <- MASS::mvrnorm(n,mu,Cov)
    exp_val <- X%*%beta
    prob <- exp(exp_val)/(1+exp(exp_val))
    y <- rbinom(n,1,prob)
    loading <- MASS::mvrnorm(1,mu,Cov)
    Est = SIHR::LF_logistic(X = X, y = y, loading = loading, intercept = TRUE, weight = rep(1,n))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #> [1] 0.005847788

    ### Standard error 

    Est$se
    #>           [,1]
    #> [1,] 0.6305518

    ### Confidence interval
    Est$CI
    #> [1] 0.0004964434 0.0651246987

    ### test whether the case probability is below 0.5 or not (1 indicates that it is above 0.5)

    Est$decision
    #> [1] 0

Individualised Treatment Effect in high-dimensional logistic model

    A1gen <- function(rho,p){
    A1=matrix(0,p,p)
    for(i in 1:p){
     for(j in 1:p){
       A1[i,j]<-rho^(abs(i-j))
     }
    }
    A1
    }
    n1 = 100
    n2 = 100
    p = 400
    mu <- rep(0,p)
    rho = 0.5
    Cov <- (A1gen(rho,p))/2
    beta1 <- rep(0,p)
    beta1[1:10] <- c(1:10)/5
    beta2 <- rep(0,p)
    beta2[1:5] <- c(1:5)/10
    X1 <- MASS::mvrnorm(n1,mu,Cov)
    X2 <- MASS::mvrnorm(n2,mu,Cov)
    exp_val1 <- X1%*%beta1
    exp_val2 <- X2%*%beta2
    prob1 <- exp(exp_val1)/(1+exp(exp_val1))
    prob2 <- exp(exp_val2)/(1+exp(exp_val2))
    y1 <- rbinom(n1,1,prob1)
    y2 <- rbinom(n2,1,prob2)
    loading <- MASS::mvrnorm(1,mu,Cov)
    Est <- SIHR::ITE_Logistic(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
    #> [1] "step is 3"
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #> [1] 0.03652782

    ### Standard error 

    Est$se
    #>          [,1]
    #> [1,] 1.014449

    ### Confidence interval
    Est$CI
    #> [1] -1.951755  2.024810

    ### test whether the first case probability is smaller than the second case probability or not (1 indicates that the first case probability is larger than the second case probability)

    Est$decision
    #> [1] 0

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

    Est = SIHR::QF(X = X, y = y, G=test.set)
    #> [1] "step is 4"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.3808362

    ### Standard error 

    Est$se
    #> [1] 0.120359

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1449369 0.6167355

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:400,400))
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 9.381411

    ### Standard error 

    Est$se
    #> [1] 1.807497

    ### Confidence interval
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 5.838781 12.92404

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE)
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.2467716

    ### Standard error 

    Est$se
    #> [1] 0.1103079

    ### Confidence interval
    Est$CI
    #>            [,1]     [,2]
    #> [1,] 0.03057218 0.462971

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

Finding projection direction in high dimensional linear regression


    n = 100
    p = 400
    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
    resol = 1.5
    step = 3

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)})

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  8.934089e-01 -3.792076e-21 -1.809754e-21 -3.604005e-22 -2.344109e-21
    #>  [6]  8.998366e-21  9.675612e-21  3.836381e-21  1.819718e-02  1.245082e-21
    #> [11] -7.309049e-22  2.588294e-02 -4.007687e-21 -1.096428e-21  1.130510e-20
    #> [16] -1.679562e-21  7.201444e-22  2.720886e-21 -6.966834e-21  2.649239e-21

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  8.934089e-01 -3.875131e-21 -1.817542e-21 -3.995235e-22 -2.317778e-21
    #>  [6]  9.179192e-21  9.841093e-21  3.941896e-21  1.819718e-02  1.276305e-21
    #> [11] -7.370245e-22  2.588294e-02 -3.938085e-21 -1.134641e-21  1.147361e-20
    #> [16] -1.690017e-21  7.774226e-22  2.730161e-21 -7.049965e-21  2.637978e-21

Finding projection direction in high dimensional logistic regression


    n = 50
    p = 400
    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow=n,ncol=p)
    y = rbinom(n,1,0.5)
    col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X)+0.0001);
    Xnor <- X %*% diag(col.norm);
    fit = glmnet::cv.glmnet(Xnor, y, alpha=1,family = "binomial")
    htheta <- as.vector(coef(fit, s = "lambda.min"))
    support<-(abs(htheta)>0.001)
    Xb <- cbind(rep(1,n),Xnor);
    Xc <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
    xnew = c(1,rep(0,(p-1)))
    loading=rep(0,pp)
    loading[1]=1
    loading[-1]=xnew
    htheta <- htheta*col.norm;
    htheta <- as.vector(htheta)
    f_prime <- exp(Xc%*%htheta)/(1+exp(Xc%*%htheta))^2

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)},model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.891777e-01  3.123477e-01 -4.065829e-22 -2.085082e-22  1.365921e-22
    #>  [6] -4.074561e-22 -1.444805e-22 -3.401175e-22 -3.617155e-22  1.893626e-22
    #> [11] -3.913237e-22 -2.928029e-22  1.459215e-22 -5.250316e-22  2.437318e-22
    #> [16]  1.349181e-22 -2.218797e-22 -2.386663e-22 -3.924442e-23 -7.324314e-23

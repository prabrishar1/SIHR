
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals (LF) and quadratic
functionals (QF) in linear regression, (2) linear functional in logistic
regression (LF_logistic), (3) individual treatment effects (ITE) in linear and
logistic regression.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("prabrishar1/SIHR")

## Example

These are basic examples which show how to use the package to conduct 
high-dimensional inference:

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
    #>          [,1]
    #> [1,] 7.201888

    ### Standard error 

    Est$se
    #> [1] 1.934024

    ### Confidence interval
    Est$CI
    #> [1]  3.41127 10.99251

    ### test whether the linear functional is below zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

Individualized Treatment Effect in high-dimensional logistic regression
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
    #>           [,1]
    #> [1,] -4.933254

    ### Standard error 

    Est$se
    #> [1] 2.544137

    ### Confidence interval
    Est$CI
    #> [1] -9.91967164  0.05316355

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
    Cov2<-matrix(NA,nrow=p,ncol=p)
    for(i in 1:p){
        for(j in 1:p){
        Cov2[i,j]<-0.5^(1+abs(i-j))
        } 
      }
    beta <- rep(0,p)
    beta[1:10] <-0.5*c(1:10)/10
    X <- MASS::mvrnorm(n,mu,Cov)
    exp_val <- X%*%beta
    prob <- exp(exp_val)/(1+exp(exp_val))
    y <- rbinom(n,1,prob)
    loading <- MASS::mvrnorm(1,mu,Cov2)
    Est = SIHR::LF_logistic(X = X, y = y, loading = loading, intercept = TRUE, weight = rep(1,n))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #> [1] 0.4316879

    ### Standard error 

    Est$se
    #>           [,1]
    #> [1,] 0.6485316

    ### Confidence interval
    Est$CI
    #> [1] 0.05640361 0.90612637

    ### test whether the case probability is below 0.5 or not (1 indicates that it is above 0.5)

    Est$decision
    #> [1] 0

Individualized Treatment Effect in high-dimensional logistic model

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
    #> [1] 0.4426277

    ### Standard error 

    Est$se
    #>           [,1]
    #> [1,] 0.8172937

    ### Confidence interval
    Est$CI
    #> [1] -1.159239  2.044494

    ### test whether the first case probability is smaller than the second case probability or not (1 indicates that the first case probability is larger than the second case probability)

    Est$decision
    #> [1] 1

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
    #> [1] "step is 5"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.6896886

    ### Standard error 

    Est$se
    #> [1] 0.1411606

    ### Confidence interval
    Est$CI
    #>          [,1]      [,2]
    #> [1,] 0.413019 0.9663582

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:400,400))
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] "step is 5"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 19.01264

    ### Standard error 

    Est$se
    #> [1] 2.412881

    ### Confidence interval
    Est$CI
    #>          [,1]    [,2]
    #> [1,] 14.28348 23.7418

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE)
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] "step is 5"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 0.401667

    ### Standard error 

    Est$se
    #> [1] 0.1158121

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1746794 0.6286545

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

Constructing projection directions in high dimensional linear regression


    n = 100
    p = 400
    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
    resol = 1.5
    step = 3

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)})

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  1.219006e+00 -1.863506e-20 -5.951143e-21 -9.733998e-22 -4.150478e-21
    #>  [6]  8.169505e-02  3.111971e-21 -7.455498e-21  5.202361e-21 -1.215674e-20
    #> [11]  1.411756e-20 -1.554899e-21 -5.525592e-21  2.928243e-21 -4.692882e-21
    #> [16] -4.683409e-21  1.837901e-20 -2.595092e-21  1.275243e-20 -1.363453e-01

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  1.219006e+00 -1.855709e-20 -5.936970e-21 -9.968212e-22 -4.142387e-21
    #>  [6]  8.169505e-02  3.113345e-21 -7.425863e-21  5.186845e-21 -1.215231e-20
    #> [11]  1.407508e-20 -1.577103e-21 -5.474844e-21  2.942562e-21 -4.689394e-21
    #> [16] -4.639912e-21  1.825912e-20 -2.543917e-21  1.264617e-20 -1.363453e-01

Constructing projection directions in high dimensional logistic regression


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
    #>  [1]  1.493989e+00 -2.630972e-20 -8.716890e-20 -2.472090e-01  3.015969e-19
    #>  [6]  1.950913e-03 -6.563375e-20 -1.924222e-20 -2.304296e-19  1.999689e-19
    #> [11] -5.646258e-20 -1.372131e-01  2.618685e-20  1.122307e-19  2.812996e-19
    #> [16]  2.922945e-21  1.765690e-20  2.621264e-20  5.658853e-20  4.759133e-20

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.842416e-01  3.189588e-01  3.047882e-22  2.254978e-22 -5.224985e-22
    #>  [6]  4.836602e-23  3.315918e-22 -3.544576e-22 -1.018956e-22  7.304296e-22
    #> [11]  2.933804e-22 -1.649531e-22 -4.543796e-22  1.005291e-22 -7.182578e-23
    #> [16]  5.722510e-23  2.225512e-22 -8.178783e-23 -1.494641e-22 -1.900137e-22

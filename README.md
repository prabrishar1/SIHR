
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
    #> [1,] -1.472131

    ### Standard error 

    Est$se
    #> [1] 2.453395

    ### Confidence interval
    Est$CI
    #> [1] -6.280697  3.336434

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
    #>          [,1]
    #> [1,] -1.80703

    ### Standard error 

    Est$se
    #> [1] 2.651258

    ### Confidence interval
    Est$CI
    #> [1] -7.003400  3.389339

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
    Est = SIHR::LF_logistic(X = X, y = y, loading = loading, weight = rep(1,n), trans = TRUE)
    #> [1] "step is 3"

    ### trans = TRUE implies target quantity is the case probability

    ### Point esitmator

    Est$prop.est
    #> [1] 0.9323544

    ### Standard error 

    Est$se
    #> [1] 0.1662482

    ### Confidence interval
    Est$CI
    #> [1] 0.07290314 0.99958623

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
    Est <- SIHR::ITE_Logistic(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, weight = NULL, trans = FALSE)
    #> [1] "step is 3"
    #> [1] "step is 3"

    ### trans = FALSE implies the target quantity is the difference between two linear combinations of the regression coefficients

    ### Point esitmator

    Est$prop.est
    #> [1] 3.916573

    ### Standard error 

    Est$se
    #> [1] 4.992152

    ### Confidence interval
    Est$CI
    #> [1] -5.867865 13.701010

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
    #> [1,] 0.6190974

    ### Standard error 

    Est$se
    #> [1] 0.1408278

    ### Confidence interval
    Est$CI
    #>         [,1]      [,2]
    #> [1,] 0.34308 0.8951149

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:400,400))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 13.95282

    ### Standard error 

    Est$se
    #> [1] 1.961894

    ### Confidence interval
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 10.10758 17.79807

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(ncol(X)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.3604669

    ### Standard error 

    Est$se
    #> [1] 0.1134924

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1380258 0.5829079

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
    #>  [1]  4.885238e-01 -1.307729e-22 -4.820590e-22 -4.730563e-22  4.099459e-22
    #>  [6] -2.121057e-22  1.107240e-22  7.074245e-22 -3.298831e-22 -8.909411e-22
    #> [11]  3.190602e-22 -5.479171e-22 -1.459483e-22  7.406262e-22  3.444757e-22
    #> [16] -4.639920e-22  5.241628e-22  2.439795e-22 -6.310831e-23  3.374623e-22

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  4.885238e-01 -1.091991e-22 -4.462848e-22 -4.090694e-22  3.788719e-22
    #>  [6] -2.294092e-22  8.874104e-23  6.652129e-22 -2.989711e-22 -7.574834e-22
    #> [11]  2.808130e-22 -4.763676e-22 -1.261935e-22  6.774587e-22  3.529624e-22
    #> [16] -4.617330e-22  4.629765e-22  2.600671e-22 -5.794442e-23  3.159084e-22

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
    #>  [1]  6.511117e-01  4.123117e-22  7.241358e-22  7.602244e-22  2.299752e-23
    #>  [6] -4.955183e-23 -8.210490e-22 -8.675124e-22 -5.280283e-22 -5.776955e-22
    #> [11]  5.245559e-22  4.773688e-22 -6.437349e-22 -2.290232e-22 -3.748980e-22
    #> [16]  8.661947e-23  1.961746e-22 -1.045093e-21 -4.234274e-23  9.773197e-23

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.115592e-01  2.276609e-01 -1.185920e-23  5.761868e-23 -1.308618e-22
    #>  [6] -6.694081e-23 -1.811012e-22 -1.467710e-22 -3.668997e-22 -9.046264e-23
    #> [11] -1.098490e-22  6.921171e-23  2.515489e-22 -1.454993e-22 -3.883868e-22
    #> [16] -7.668592e-23  9.941822e-23  1.244238e-22  3.409436e-24  2.887665e-23

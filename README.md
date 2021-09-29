
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
    #> [1,] 0.8117898

    ### Standard error 

    Est$se
    #> [1] 1.984966

    ### Confidence interval
    Est$CI
    #> [1] -3.078672  4.702252

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
    #> [1,] 5.942159

    ### Standard error 

    Est$se
    #> [1] 2.185656

    ### Confidence interval
    Est$CI
    #> [1]  1.658353 10.225965

    ### test whether the linear ITE is below zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

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
    #> [1] 0.1619696

    ### Standard error 

    Est$se
    #> [1] 0.4161587

    ### Confidence interval
    Est$CI
    #> [1] 0.0004744842 0.9874513689

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
    #> [1] 2.358865

    ### Standard error 

    Est$se
    #> [1] 4.541018

    ### Confidence interval
    Est$CI
    #> [1] -6.541366 11.259095

    ### test whether the first case probability is smaller than the second case probability or not (1 indicates that the first case probability is larger than the second case probability)

    Est$decision
    #> [1] 0

Inference for single regression coefficient in high-dimensional binary
generalized linear model


    sp = 20
    n = 400
    p = 800

    sig1 = toeplitz(seq(0.6, 0,length.out = p/10))
    Sig = Matrix::bdiag(rep(list(sig1),10))+diag(rep(0.4,p))
    X = MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
    b = rep(0,p)
    b[1:sp] = rep(c(0.4,-0.4), sp/2)

    ## Inference for single regression coefficient in high-dimensional binary probit model
    f = function(x){
      pnorm(x)
    }
    prob = f(X %*% b)
    y = rep(1,n)
    while(sum(y)/n<0.02 | sum(y)/n>0.98 ){
      for(gen.y in 1:n){
      y[gen.y] = rbinom(1,1,prob[gen.y])
     }
    }
    Est = SIHR::GLM_binary(X = X, y = y, loading = 1, model = "probit", intercept = FALSE)
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #> [1] 0.4678901

    ### Standard error 

    Est$se
    #> [1] 0.1362048

    ### Confidence interval
    Est$CI
    #> [1] 0.2009336 0.7348467

    ### test whether the first regression coefficient is equal to zero or not (1 indicates that it is significantly different from zero)

    Est$decision
    #> [1] 1

    ## Inference for single regression coefficient in high-dimensional binary inverse t_1 model
    f = function(x){
      pt(x,1)
    }
    prob = f(X %*% b)
    y = rep(1,n)
    while(sum(y)/n<0.02 | sum(y)/n>0.98 ){
      for(gen.y in 1:n){
      y[gen.y] = rbinom(1,1,prob[gen.y])
     }
    }
    Est = SIHR::GLM_binary(X = X, y = y, loading = 2, model = "inverse t1", intercept = FALSE, lambda=0.1*sqrt(log(p)/n))
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #> [1] -0.1388372

    ### Standard error 

    Est$se
    #> [1] 0.2015797

    ### Confidence interval

    Est$CI
    #> [1] -0.5339261  0.2562517

    ### test whether the second regression coefficient is equal to zero or not (1 indicates that it is significantly different from zero)

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
    #> [1] "step is 5"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.3816919

    ### Standard error 

    Est$se
    #> [1] 0.1246856

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1373127 0.6260712

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
    #> [1] "step is 2"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 5.104558

    ### Standard error 

    Est$se
    #> [1] 0.9818775

    ### Confidence interval
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 3.180114 7.029003

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.2784269

    ### Standard error 

    Est$se
    #> [1] 0.104981

    ### Confidence interval
    Est$CI
    #>            [,1]      [,2]
    #> [1,] 0.07266788 0.4841859

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
    #>  [1]  5.943949e-01 -2.514259e-22 -9.178186e-22  2.245324e-22 -1.925582e-21
    #>  [6] -1.213352e-21 -1.596504e-22 -2.579478e-22  4.371180e-22 -8.751323e-22
    #> [11]  5.347556e-23  5.396900e-22  5.188099e-22 -2.035144e-21 -6.921150e-22
    #> [16] -1.062399e-21  1.651793e-21  6.128073e-02  1.593045e-21 -1.080459e-21

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  5.943949e-01 -2.065718e-22 -9.278581e-22  2.245584e-22 -1.939450e-21
    #>  [6] -1.206573e-21 -1.856260e-22 -2.743166e-22  4.910344e-22 -8.588489e-22
    #> [11]  5.476096e-23  5.278244e-22  4.857569e-22 -2.025154e-21 -7.194241e-22
    #> [16] -1.122114e-21  1.656377e-21  6.128073e-02  1.603681e-21 -1.090778e-21

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
    #>  [1]  1.403063e+00  1.021032e-19 -2.028876e-20  2.661605e-20  1.383991e-19
    #>  [6]  3.273462e-20  3.859118e-20  6.026510e-20 -6.175705e-02  4.573411e-20
    #> [11]  6.404736e-20  1.014144e-19  8.847057e-21  1.035777e-19  4.873431e-21
    #> [16] -7.674316e-21 -1.142493e-19 -4.968120e-02  1.193482e-20 -5.864998e-20

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.242675e-01  2.934330e-01  1.802700e-22 -3.222157e-22 -9.701420e-23
    #>  [6]  4.211452e-22  2.356984e-22 -1.808702e-22 -2.079894e-22 -6.410890e-22
    #> [11]  1.012583e-22  3.064627e-22  2.897866e-22 -2.997909e-22 -2.992561e-23
    #> [16]  4.096905e-23  2.314291e-22 -2.890201e-22 -4.278986e-22 -9.312300e-23


<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1)linear functionals (LF) and quadratic
functionals (QF) in linear regression, (2)linear functional in logistic
regression, (3) individual treatment effects (ITE) in linear and
logistic regression and (4) single regression coefficient in binary
outcome regression.

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
    Est = SIHR::LF(X = X, y = y, loading = loading)
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] -7.750703

    ### Standard error 

    Est$se
    #> [1] 2.169246

    ### Confidence interval
    Est$CI
    #> [1] -12.002347  -3.499059

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
    Est <- SIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading)
    #> [1] "step is 3"
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] -5.581559

    ### Standard error 

    Est$se
    #> [1] 2.499677

    ### Confidence interval
    Est$CI
    #> [1] -10.4808351  -0.6822831

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
    #> [1] 0.3264385

    ### Standard error 

    Est$se
    #> [1] 0.7664956

    ### Confidence interval
    Est$CI
    #> [1] 0.000522256 0.997780286

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
    #> [1] -1.409946

    ### Standard error 

    Est$se
    #> [1] 4.766363

    ### Confidence interval
    Est$CI
    #> [1] -10.751845   7.931954

    ### test whether the first case probability is smaller than the second case probability or not (1 indicates that the first case probability is larger than the second case probability)

    Est$decision
    #> [1] 0

Inference for single regression coefficient in high-dimensional binary
GLM (probit model)


    sp = 20
    n = 500
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
    y = array(dim = 1)
    for(i in 1:n){
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_binary(X = X, y = y,index = 1, model = "probit", intercept = FALSE)
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #> [1] 0.2076012

    ### Standard error 

    Est$se
    #> [1] 0.1054046

    ### Confidence interval
    Est$CI
    #> [1] 0.001011965 0.414190351

    ### test whether the first regression coefficient is equal to zero or not (1 indicates that it is significantly different from zero)

    Est$decision
    #> [1] 1

Inference for single regression coefficient in high-dimensional binary
GLM (inverse t1 model)


    sp = 10
    n = 800
    p = 400

    sig1 = toeplitz(seq(0.6, 0,length.out = p/10))
    Sig = Matrix::bdiag(rep(list(sig1),10))+diag(rep(0.4,p))
    X = MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
    b = rep(0,p)
    b[1:sp] = rep(c(0.4,-0.4), sp/2)
    f = function(x){
      pt(x,1)
    }
    prob = f(X %*% b)
    y = array(dim = 1)
    for(i in 1:n){
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_binary(X = X, y = y, index = 2, model = "inverse t1", lambda=0.1*sqrt(log(p)/n))
    #> [1] "step is 4"

    ### Point esitmator

    Est$prop.est
    #> [1] -0.5499067

    ### Standard error 

    Est$se
    #> [1] 0.1211397

    ### Confidence interval

    Est$CI
    #> [1] -0.7873360 -0.3124773

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
    #> [1] "step is 4"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.3579101

    ### Standard error 

    Est$se
    #> [1] 0.1187189

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1252253 0.5905949

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 2.898267

    ### Standard error 

    Est$se
    #> [1] 0.8041309

    ### Confidence interval
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 1.322199 4.474334

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.2738387

    ### Standard error 

    Est$se
    #> [1] 0.1093765

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.0594647 0.4882128

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
    #>  [1]  5.607283e-01 -5.595600e-22  1.235536e-21  2.449935e-22 -6.992049e-03
    #>  [6] -4.504528e-23 -1.482542e-22 -1.521506e-22 -2.654100e-22 -1.996896e-22
    #> [11]  4.154248e-22 -4.927661e-23 -8.973232e-24 -7.282273e-23  9.908534e-22
    #> [16]  4.930162e-22  5.664041e-22 -8.152939e-22  1.409246e-22  6.194871e-22

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  5.607283e-01 -5.778937e-22  1.312541e-21  2.545603e-22 -6.992049e-03
    #>  [6] -2.854664e-23 -1.693089e-22 -1.653340e-22 -2.686916e-22 -1.826828e-22
    #> [11]  4.938496e-22 -2.039571e-23  2.394669e-23 -7.415389e-23  1.051351e-21
    #> [16]  4.818045e-22  5.965234e-22 -8.930813e-22  1.384202e-22  6.142351e-22

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
    step <- 2

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)},model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.298132e-01 -6.354954e-23 -6.014956e-23  1.422227e-23 -3.172897e-23
    #>  [6]  1.823724e-23  3.114810e-23  1.542873e-23 -6.710611e-23  3.725745e-23
    #> [11] -5.090520e-23  7.257707e-23  1.053316e-23  4.912644e-23  1.228531e-23
    #> [16]  1.182191e-23 -5.913138e-23 -1.722514e-23  3.438262e-23  5.472930e-24

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.486979e-01  2.596627e-01 -1.194396e-22 -2.271496e-22 -3.375731e-22
    #>  [6] -9.876575e-23 -3.244736e-23  8.373347e-23 -1.083039e-22 -1.309022e-22
    #> [11]  1.742417e-22  2.164754e-22  4.052596e-22 -4.009467e-23  1.013356e-22
    #> [16] -1.488862e-23  1.134927e-22 -2.012654e-22  2.812060e-22  4.417586e-23

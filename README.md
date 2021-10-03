
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1)linear functionals (LF) (Cai, Cai, and
Guo 2019) and quadratic functionals (QF)(Guo, Renaux, et al. 2019) in
linear regression, (2)linear functional in logistic regression (Guo,
Rakshit, et al. 2019), (3) individual treatment effects (ITE) in linear
and logistic regression and (4) single regression coefficient in binary
outcome regression (Cai, Guo, and Ma, n.d.).

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
    #> [1,] -2.710286

    ### Standard error 

    Est$se
    #> [1] 1.999067

    ### Confidence interval
    Est$CI
    #> [1] -6.628384  1.207813

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
    #>          [,1]
    #> [1,] 1.761438

    ### Standard error 

    Est$se
    #> [1] 3.142746

    ### Confidence interval
    Est$CI
    #> [1] -4.398232  7.921107

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
    #> [1] 0.4010301

    ### Standard error 

    Est$se
    #> [1] 0.740089

    ### Confidence interval
    Est$CI
    #> [1] 0.001593916 0.996451292

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
    #> [1] -2.351335

    ### Standard error 

    Est$se
    #> [1] 4.459392

    ### Confidence interval
    Est$CI
    #> [1] -11.091582   6.388912

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
    #> [1] 0.3533391

    ### Standard error 

    Est$se
    #> [1] 0.1180848

    ### Confidence interval
    Est$CI
    #> [1] 0.1218971 0.5847812

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
    #> [1] -0.3399147

    ### Standard error 

    Est$se
    #> [1] 0.1094344

    ### Confidence interval

    Est$CI
    #> [1] -0.5544023 -0.1254271

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
    #>          [,1]
    #> [1,] 0.506281

    ### Standard error 

    Est$se
    #> [1] 0.1226881

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.2658167 0.7467453

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 2.535089

    ### Standard error 

    Est$se
    #> [1] 0.8902969

    ### Confidence interval
    Est$CI
    #>           [,1]     [,2]
    #> [1,] 0.7901388 4.280039

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.3121318

    ### Standard error 

    Est$se
    #> [1] 0.1074686

    ### Confidence interval
    Est$CI
    #>           [,1]      [,2]
    #> [1,] 0.1014973 0.5227663

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
    #>  [1]  6.087440e-01  9.047431e-23 -1.775909e-22  5.770540e-22 -4.086164e-22
    #>  [6]  7.823024e-22 -1.700983e-22 -3.492342e-04  1.020460e-21  2.039530e-22
    #> [11]  2.612214e-22  2.560820e-22  1.338079e-22 -9.545500e-22  2.763455e-22
    #> [16] -3.739623e-22  2.444999e-22 -4.943140e-22 -8.236880e-03 -6.490740e-22

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  6.087440e-01  8.469122e-23 -1.921934e-22  5.829067e-22 -4.156921e-22
    #>  [6]  7.819023e-22 -2.031212e-22 -3.492342e-04  1.050463e-21  2.059268e-22
    #> [11]  2.187954e-22  2.659240e-22  1.356361e-22 -9.403553e-22  2.998784e-22
    #> [16] -3.376842e-22  2.598785e-22 -4.518358e-22 -8.236880e-03 -6.673704e-22

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
    #>  [1]  3.397445e-01  3.729277e-23 -1.927974e-22 -7.233107e-23 -8.796245e-23
    #>  [6] -2.753112e-23  7.384742e-23  1.825561e-23  4.129437e-23 -7.956405e-23
    #> [11]  1.023839e-22 -5.348840e-23 -3.352030e-23 -5.166470e-23 -8.391276e-24
    #> [16] -5.683316e-23 -1.449247e-22 -3.293484e-23 -2.333875e-23 -4.130985e-23

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  4.092802e-01  3.144957e-01  3.371496e-22 -4.142656e-22  2.160712e-23
    #>  [6]  1.368043e-22  4.034595e-22  2.829536e-22 -8.999946e-23  4.051203e-23
    #> [11] -7.194496e-22  4.629574e-22 -5.095783e-22  8.882246e-23 -2.220811e-22
    #> [16] -2.410226e-22 -1.740991e-22 -1.703005e-22 -1.028864e-23 -2.266020e-22

<div id="refs" class="references hanging-indent">

<div id="ref-linlin">

Cai, Tianxi, T. Tony Cai, and Zijian Guo. 2019. “Optimal Statistical
Inference for Individualized Treatment Effects in High-Dimensional
Models.” *Journal of the Royal Statistical Society: Series B*.
<https://arxiv.org/pdf/1904.12891.pdf>.

</div>

<div id="ref-glm">

Cai, T Tony, Zijian Guo, and Rong Ma. n.d. “Statistical Inference for
High-Dimensional Generalized Linear Models with Binary Outcomes.”
*Journal of the American Statistical Association*.

</div>

<div id="ref-linlog">

Guo, Zijian, Prabrisha Rakshit, Daniel S. Herman, and Jinbo Chen. 2019.
“Inference for Case Probability in High-Dimensional Logistic
Regression.” *Unknown*. <https://arxiv.org/abs/2012.07133>.

</div>

<div id="ref-grouplin">

Guo, Zijian, Claude Renaux, Peter Buhlmann, and T. Tony Cai. 2019.
“Group Inference in High Dimensions with Applications to Hierarchical
Testing.” *Unknown*. <https://arxiv.org/pdf/1909.01503.pdf>.

</div>

</div>

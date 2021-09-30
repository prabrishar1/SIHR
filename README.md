
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
outcome regression (Cai, Guo, and Ma 2021).

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
    #>          [,1]
    #> [1,] 2.260871

    ### Standard error 

    Est$se
    #> [1] 2.06547

    ### Confidence interval
    Est$CI
    #> [1] -1.787376  6.309119

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
    #> [1,] -1.20696

    ### Standard error 

    Est$se
    #> [1] 2.557417

    ### Confidence interval
    Est$CI
    #> [1] -6.219406  3.805486

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
    #> [1] 0.1270018

    ### Standard error 

    Est$se
    #> [1] 0.3181829

    ### Confidence interval
    Est$CI
    #> [1] 0.0005245413 0.9758021845

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
    #> [1] 0.2507293

    ### Standard error 

    Est$se
    #> [1] 5.837083

    ### Confidence interval
    Est$CI
    #> [1] -11.18974  11.69120

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
    y = array(dim = 1)
    for(i in 1:n){
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_binary(X = X, y = y,index = 1, model = "probit", intercept = FALSE)
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #> [1] 0.6239

    ### Standard error 

    Est$se
    #> [1] 0.1329187

    ### Confidence interval
    Est$CI
    #> [1] 0.3633842 0.8844159

    ### test whether the first regression coefficient is equal to zero or not (1 indicates that it is significantly different from zero)

    Est$decision
    #> [1] 1

    ## Inference for single regression coefficient in high-dimensional binary inverse t_1 model
    f = function(x){
      pt(x,1)
    }
    prob = f(X %*% b)
    y = array(dim = 1)
    for(i in 1:n){
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_binary(X = X, y = y, index = 2, model = "inverse t1", intercept = FALSE, lambda=0.1*sqrt(log(p)/n))
    #> [1] "step is 3"

    ### Point esitmator

    Est$prop.est
    #> [1] -0.3261996

    ### Standard error 

    Est$se
    #> [1] 0.1724078

    ### Confidence interval

    Est$CI
    #> [1] -0.66411256  0.01171346

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
    #> [1,] 0.2719515

    ### Standard error 

    Est$se
    #> [1] 0.1124773

    ### Confidence interval
    Est$CI
    #>            [,1]      [,2]
    #> [1,] 0.05150014 0.4924029

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 3.851185

    ### Standard error 

    Est$se
    #> [1] 1.268083

    ### Confidence interval
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 1.365789 6.336581

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
    #> [1] "step is 3"
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 0.178718

    ### Standard error 

    Est$se
    #> [1] 0.1118829

    ### Confidence interval
    Est$CI
    #>             [,1]      [,2]
    #> [1,] -0.04056839 0.3980045

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    Est$decision
    #> [1] 0

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
    #>  [1]  7.458383e-01  9.835931e-22  1.745687e-21 -1.121046e-23  1.715649e-02
    #>  [6]  8.302776e-23 -1.264732e-21  4.432225e-22  2.123002e-21 -7.567140e-23
    #> [11]  1.551464e-22 -1.850696e-21 -2.035603e-21  2.631413e-21 -2.537197e-21
    #> [16] -2.276756e-22  1.609136e-21  5.021932e-23 -6.697350e-22 -3.775395e-21

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  7.458383e-01  9.676540e-22  1.705290e-21  3.379367e-23  1.715649e-02
    #>  [6]  7.244085e-23 -1.274056e-21  4.886569e-22  2.156613e-21 -8.253717e-23
    #> [11]  1.626689e-22 -1.809575e-21 -2.034243e-21  2.616713e-21 -2.461693e-21
    #> [16] -2.399082e-22  1.539481e-21  5.703314e-23 -6.378915e-22 -3.735850e-21

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
    #>  [1]  3.354298e-01  3.354298e-01  5.484228e-04 -4.517460e-23 -1.183126e-22
    #>  [6]  1.995973e-22  8.708339e-23  2.581457e-22 -2.406489e-22  2.155913e-22
    #> [11] -1.425415e-23  4.511503e-23 -3.236028e-22 -1.304510e-22 -1.191209e-22
    #> [16]  5.664993e-23 -3.284727e-22 -4.268898e-23 -2.607856e-22  3.780519e-24

<div id="refs" class="references hanging-indent">

<div id="ref-linlin">

Cai, Tianxi, T. Tony Cai, and Zijian Guo. 2019. “Optimal Statistical
Inference for Individualized Treatment Effects in High-Dimensional
Models.” *To Appear in JRSSB*. <https://arxiv.org/pdf/1904.12891.pdf>.

</div>

<div id="ref-glm">

Cai, T Tony, Zijian Guo, and Rong Ma. 2021. “Statistical Inference for
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

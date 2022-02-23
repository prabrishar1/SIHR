
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals in generalized
linear regression (GLM\_LF), (2) quadratic functionals in linear
regression (QF) (3) individual treatment effects in generalized linear
regression (ITE). \#\# Installation

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

    set.seed(1203)

    X <- MASS::mvrnorm(n,mu,Cov)

    set.seed(1203)

    y = X%*%beta + rnorm(n)

    set.seed(1203)

    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est = SIHR::GLM_LF(X = X, y = y, loading = loading)
    #> step is 3

    ### Point esitmator

    Est$prop.est
    #> [1] -2.260657

    ### Standard error 

    Est$se
    #> [1] 2.418408

    ### Confidence interval
    SIHR::inf(Est)$CI
    #> [1] -7.000649  2.479335

    ### test whether the linear functional is below zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
    #> [1] 0

Individualised Treatment Effect in high-dimensional linear regression
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

    set.seed(1203)

    X1 <- MASS::mvrnorm(n1,mu,Cov)

    set.seed(1203)

    X2 <- MASS::mvrnorm(n2,mu,Cov)

    set.seed(1203)

    y1 = X1%*%beta1 + rnorm(n1)

    set.seed(1203)

    y2 = X2%*%beta2 + rnorm(n2)

    set.seed(1203)

    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est <- SIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2, loading = loading)
    #> step is 3step is 3
    ### Point esitmator

    Est$prop.est
    #> [1] -3.655104

    ### Standard error 

    Est$se
    #> [1] 2.526009

    ### Confidence interval
    SIHR::inf(Est)$CI
    #> [1] -8.605990  1.295783

    ### test whether the linear ITE is below zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
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

    set.seed(1203)

    X <- MASS::mvrnorm(n,mu,Cov)
    exp_val <- X%*%beta
    prob <- exp(exp_val)/(1+exp(exp_val))

    set.seed(1203)

    y <- rbinom(n,1,prob)

    set.seed(1203)

    loading <- MASS::mvrnorm(1,mu,Cov)
    Est = SIHR::GLM_LF(X = X, y = y, model = "logistic", loading = loading)
    #> step is 3

    ### Point esitmator

    Est$prop.est
    #> [1] -1.857657

    ### Standard error 

    Est$se
    #> [1] 0.2081954

    ### Confidence interval
    SIHR::inf(Est)$CI
    #> [1] -2.265713 -1.449602

    ### test whether the linear functional is below zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
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

    set.seed(1203)

    X1 <- MASS::mvrnorm(n1,mu,Cov)

    set.seed(1203)

    X2 <- MASS::mvrnorm(n2,mu,Cov)
    exp_val1 <- X1%*%beta1
    exp_val2 <- X2%*%beta2
    prob1 <- exp(exp_val1)/(1+exp(exp_val1))
    prob2 <- exp(exp_val2)/(1+exp(exp_val2))

    set.seed(1203)

    y1 <- rbinom(n1,1,prob1)

    set.seed(1203)

    y2 <- rbinom(n2,1,prob2)

    set.seed(1203)

    loading <- MASS::mvrnorm(1,mu,Cov)
    Est <- SIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2, model = "logistic alternative", loading = loading)
    #> step is 3step is 3

    ### model = "logistic alternative" implies the weight vector is inverse of the derivative of the logit function

    ### Point esitmator

    Est$prop.est
    #> [1] -0.44295

    ### Standard error 

    Est$se
    #> [1] 5.071368

    ### Confidence interval
    SIHR::inf(Est)$CI
    #> [1] -10.38265   9.49675

    ### test whether the first linear functional is smaller than the second linear functional or not (1 indicates that the first linear functional is larger than the second linear functional)

    SIHR::inf(Est)$decision
    #> [1] 0

Inference for linear functional in high-dimensional probit model


    sp = 20
    n = 500
    p = 800

    sig1 = toeplitz(seq(0.6, 0,length.out = p/10))
    Sig = Matrix::bdiag(rep(list(sig1),10))+diag(rep(0.4,p))

    set.seed(1203)

    X = MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
    b = rep(0,p)
    b[1:sp] = rep(c(0.4,-0.4), sp/2)
    f = function(x){
      pnorm(x)
    }
    prob = f(X %*% b)
    y = array(dim = 1)
    for(i in 1:n){
    set.seed(i)
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_LF(X = X, y = y, loading = c(1,rep(0,p-1)), model = "probit", intercept = FALSE)
    #> Setting intercept = FALSE and intercept.loading = FALSEstep is 3

    ### Point esitmator

    Est$prop.est
    #> [1] 0.3635321

    ### Standard error 

    Est$se
    #> [1] 0.09325356

    ### Confidence interval
    SIHR::inf(Est)$CI
    #> [1] 0.1807585 0.5463057

    ###  test whether the first regression coefficient is below zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
    #> [1] 1

Inference for linear functional in high-dimensional inverse t1 model


    sp = 10
    n = 800
    p = 400

    sig1 = toeplitz(seq(0.6, 0,length.out = p/10))
    Sig = Matrix::bdiag(rep(list(sig1),10))+diag(rep(0.4,p))

    set.seed(1203)

    X = MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
    b = rep(0,p)
    b[1:sp] = rep(c(0.4,-0.4), sp/2)
    f = function(x){
      pt(x,1)
    }
    prob = f(X %*% b)
    y = array(dim = 1)
    for(i in 1:n){
    set.seed(i)
    y[i] = rbinom(1,1,prob[i])
    }
    Est = SIHR::GLM_LF(X = X, y = y, loading = c(0, 1, rep(0,p-2)), model = "inverse t1", lambda=0.1*sqrt(log(p)/n))
    #> step is 4

    ### Point esitmator

    Est$prop.est
    #> [1] -0.3861175

    ### Standard error 

    Est$se
    #> [1] 0.08312194

    ### Confidence interval

    SIHR::inf(Est)$CI
    #> [1] -0.5490335 -0.2232015

    ### test whether the second regression coefficient is below zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
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

    set.seed(1203)

    X <- MASS::mvrnorm(100,mu,Cov)

    set.seed(1203)

    y <- X%*%beta + rnorm(100)
    test.set <- c(30:100)

    ## Inference for Quadratic Functional with Population Covariance Matrix in middle

    Est = SIHR::QF(X = X, y = y, G=test.set)
    #> step is 5
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.5694124

    ### Standard error 

    Est$se
    #> [1] 0.1248953

    ### Confidence interval
    SIHR::inf(Est)$CI
    #>           [,1]      [,2]
    #> [1,] 0.3246221 0.8142027

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
    #> step is 3
    ### Point esitmator

    Est$prop.est
    #>          [,1]
    #> [1,] 2.178035

    ### Standard error 

    Est$se
    #> [1] 0.1738793

    ### Confidence interval
    SIHR::inf(Est)$CI
    #>          [,1]     [,2]
    #> [1,] 1.837238 2.518832

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
    #> step is 3
    ### Point esitmator

    Est$prop.est
    #>           [,1]
    #> [1,] 0.2078442

    ### Standard error 

    Est$se
    #> [1] 0.1003911

    ### Confidence interval
    SIHR::inf(Est)$CI
    #>            [,1]      [,2]
    #> [1,] 0.01108131 0.4046071

    ### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

    SIHR::inf(Est)$decision
    #> [1] 1

Finding projection direction in high dimensional linear regression


    n = 100
    p = 400

    set.seed(1203)

    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
    resol = 1.5
    step = 3

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X, loading = c(1,rep(0,(p-1))), mu = sqrt(2.01*log(p)/n)*resol^{-(step-1)})

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  9.062583e-01  4.590960e-02  3.549455e-21  3.089321e-21  5.045688e-21
    #>  [6]  2.918756e-22  2.352503e-21 -6.861654e-21  5.937544e-21  1.559207e-22
    #> [11] -5.030578e-02  2.196155e-21  2.085941e-21  4.124930e-22  1.896100e-21
    #> [16] -1.326126e-21 -1.681926e-21  3.271831e-22 -8.077548e-22 -1.478807e-21

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  9.062583e-01  4.590960e-02  3.545466e-21  3.098135e-21  5.042330e-21
    #>  [6]  3.117734e-22  2.368881e-21 -6.920453e-21  5.924238e-21  1.497638e-22
    #> [11] -5.030578e-02  2.234993e-21  2.045624e-21  3.982008e-22  1.877098e-21
    #> [16] -1.322869e-21 -1.736133e-21  3.301470e-22 -7.728802e-22 -1.444906e-21

Finding projection direction in high dimensional binary GLM (logistic
regression)


    n = 50
    p = 400

    set.seed(1203)

    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow=n,ncol=p)

    set.seed(1203)

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

    Direction.est <- SIHR::Direction_fixedtuning(X, loading = c(1,rep(0,(p-1))), mu = sqrt(2.01*log(p)/n)*resol^{-(step-1)}, model = "glm", weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.504265e-01  2.156102e-24  1.178464e-22  8.286258e-23  6.312989e-23
    #>  [6]  1.031157e-22  9.434066e-24 -1.000376e-22  6.662721e-23  6.897393e-23
    #> [11] -6.262503e-23  9.710954e-23 -4.212128e-23 -1.720586e-23  1.343001e-22
    #> [16] -1.479225e-23 -4.997709e-23  1.876474e-22  7.467477e-23 -5.604370e-23

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc, loading, model = "glm", weight = 1/f_prime, deriv.vec = f_prime)

    ### First 20 entries of the projection vector

    Direction.est$proj[1:20]
    #>  [1]  3.682569e-01  2.900132e-01 -1.687053e-22  3.294033e-22  1.576140e-22
    #>  [6]  1.320356e-22  2.025055e-22 -6.697072e-23 -1.667529e-22  3.277260e-22
    #> [11]  2.071755e-22 -6.106372e-24  2.150720e-22  1.205219e-23 -1.869162e-22
    #> [16]  2.984627e-22  9.761420e-23 -5.296210e-23  2.863307e-22  2.137952e-22

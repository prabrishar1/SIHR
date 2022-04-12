## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)

## ----example------------------------------------------------------------------
library(SIHR)

## ----Linear Functional Linear Model-------------------------------------------
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

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the linear functional is below zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----ITE Linear Model---------------------------------------------------------

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
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the linear ITE is below zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----Linear Functional Logistic Model-----------------------------------------
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

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the linear functional is below zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----ITE Logistic Model-------------------------------------------------------
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

### model = "logistic alternative" implies the weight vector is inverse of the derivative of the logit function

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the first linear functional is smaller than the second linear functional or not (1 indicates that the first linear functional is larger than the second linear functional)

SIHR::inf(Est)$decision


## ----Generalized Linear Model probit------------------------------------------

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

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

###  test whether the first regression coefficient is below zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----Generalized Linear Model inverse t1--------------------------------------

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

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval

SIHR::inf(Est)$CI

### test whether the second regression coefficient is below zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----Group Linear Model-------------------------------------------------------

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
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## Inference for Quadratic Functional with known matrix A in middle

Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## Inference for square norm of regression vector

Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----Group Logistic Model-----------------------------------------------------

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
beta <- rep(0,400)
beta[25:50] <- 0.08

set.seed(1203)

X <- MASS::mvrnorm(100,mu,Cov)
exp_val <- X%*%beta
prob <- exp(exp_val)/(1+exp(exp_val))
set.seed(1203)
y <- rbinom(100,1,prob)
test.set <- c(30:100)

## Inference for Quadratic Functional with Population Covariance Matrix in middle

Est = SIHR::QF(X = X, y = y, model = "logistic", G=test.set)
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## Inference for Quadratic Functional with known matrix A in middle

Est = SIHR::QF(X = X, y = y, model = "logistic", G=test.set, Cov.weight = FALSE,A = diag(1:length(test.set),length(test.set)))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## Inference for square norm of regression vector

Est = SIHR::QF(X = X, y = y, model = "logistic", G=test.set, Cov.weight = FALSE, A = diag(length(test.set)))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
SIHR::inf(Est)$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

SIHR::inf(Est)$decision

## ----proj linear--------------------------------------------------------------

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

## Finding Projection Direction using best step size

Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

### First 20 entries of the projection vector

Direction.est$proj[1:20]


## ----proj logistic------------------------------------------------------------

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

## Finding Projection Direction using best step size

Direction.est <- SIHR::Direction_searchtuning(Xc, loading, model = "glm", weight = 1/f_prime, deriv.vec = f_prime)

### First 20 entries of the projection vector

Direction.est$proj[1:20]



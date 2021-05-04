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
X <- MASS::mvrnorm(n,mu,Cov)
y = X%*%beta + rnorm(n)
loading <- MASS::mvrnorm(1,rep(0,p),Cov)
Est = SIHR::LF(X = X, y = y, loading = loading, intercept = TRUE)

### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the linear functional is below zero or not (1 indicates that it is above zero)

Est$decision

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
X1 <- MASS::mvrnorm(n1,mu,Cov)
X2 <- MASS::mvrnorm(n2,mu,Cov)
y1 = X1%*%beta1 + rnorm(n1)
y2 = X2%*%beta2 + rnorm(n2)
loading <- MASS::mvrnorm(1,rep(0,p),Cov)
Est <- SIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the linear ITE is below zero or not (1 indicates that it is above zero)

Est$decision

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
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the case probability is below 0.5 or not (1 indicates that it is above 0.5)

Est$decision

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
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the first case probability is smaller than the second case probability or not (1 indicates that the first case probability is larger than the second case probability)

Est$decision

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
X <- MASS::mvrnorm(100,mu,Cov)
y <- X%*%beta + rnorm(100)
test.set <- c(30:100)

## Inference for Quadratic Functional with Population Covariance Matrix in middle

Est = SIHR::QF(X = X, y = y, G=test.set)
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

Est$decision

## Inference for Quadratic Functional with known matrix A in middle

Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(1:400,400))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

Est$decision

## Inference for square norm of regression vector

Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE, A = diag(400))
### Point esitmator

Est$prop.est

### Standard error 

Est$se

### Confidence interval
Est$CI

### test whether the quadratic form is equal to zero or not (1 indicates that it is above zero)

Est$decision

## ----proj linear--------------------------------------------------------------

n = 100
p = 400
X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
resol = 1.5
step = 3

## Finding Projection Direction using fixed tuning parameter

Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)})

### First 20 entries of the projection vector

Direction.est$proj[1:20]

## Finding Projection Direction using best step size

Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))

### First 20 entries of the projection vector

Direction.est$proj[1:20]


## ----proj logistic------------------------------------------------------------

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

## Finding Projection Direction using best step size

Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)

### First 20 entries of the projection vector

Direction.est$proj[1:20]



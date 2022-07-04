
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals in generalized
linear regression, (2) individual treatment effects in generalized
linear regression (ITE), (3) quadratic functionals in generalized linear
regression (QF).

Currently, we support different generalized linear regression, by
specifying the argument `model` in “linear”, “logisitc”,
“logistic_alternative” or “probit”.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("prabrishar1/SIHR")
```

## Examples

These are basic examples which show how to solve the common
high-dimensional inference problems:

``` r
library(SIHR)
```

### Linear functional in linear regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
X = matrix(rnorm(100*120), nrow=100, ncol=120)
y = -0.5 + X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=TRUE
truth1 = -0.5 + 0.5 * 1 + 1 * 1
truth2 = -0.5 + 0.5 * -0.5 + 1 * -1
truth = c(truth1, truth2)
truth
#> [1]  1.00 -1.75
```

Call `LF` with `model="linear"`:

``` r
Est = LF(X, y, loading.mat, model="linear", intercept.loading=TRUE, verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading      lower     upper
#> 1       1  0.4819024  1.136367
#> 2       2 -2.0743781 -1.508034
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.5591     0.8091     0.1670   4.846 1.258e-06 ***
#>        2    -1.6136    -1.7912     0.1445 -12.398 2.687e-35 ***
```

### Linear functional in logistic regression model

Generate Data and find the truth linear functionals:

``` r
X = matrix(rnorm(100*120), nrow=100, ncol=120)
exp_val = -0.5 + X[,1] * 0.5 + X[,2] * 1
prob = exp(exp_val) / (1+exp(exp_val))
y = rbinom(100, 1, prob)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=TRUE
truth1 = -0.5 + 0.5 * 1 + 1 * 1
truth2 = -0.5 + 0.5 * -0.5 + 1 * -1
truth = c(truth1, truth2)
truth
#> [1]  1.00 -1.75
```

Call `LF` with `model="logistic"` or `model="logistic_alternative"`:

``` r
## model = "logisitc" or "logistic_alternative"
Est = LF(X, y, loading.mat, model="logistic", intercept.loading=TRUE, verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `LF`

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower      upper
#> 1       1 -0.5006444  1.0388104
#> 2       2 -1.9043064 -0.4210012
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.3773892 0.7386204
#> 2       2 0.1296219 0.3962772
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)   
#>        1    -0.7933     0.2691     0.3927  0.6852 0.493238   
#>        2    -0.7933    -1.1627     0.3784 -3.0725 0.002122 **
```

### Individualized Treatment Effect in linear regression model

Generate Data and find the truth linear functionals:

``` r
## 1st data
X1 = matrix(rnorm(100*120), nrow=100, ncol=120)
y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
## 2nd data
X2 = matrix(0.8*rnorm(100*120), nrow=100, ncol=120)
y2 = 0.1 + X2[,1] * 0.8 + X2[,2] * 0.8 + rnorm(100)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=TRUE
truth1 = (-0.5 + 0.5*1 + 1*1) - (0.1 + 0.8*1 + 0.8*1)
truth2 = (-0.5 + 0.5*(-0.5) + 1*(-1)) - (0.1 + 0.8*(-0.5) + 0.8*(-1))
truth = c(truth1, truth2)
truth
#> [1] -0.70 -0.65
```

Call `ITE` with `model="linear"`:

``` r
Est = ITE(X1, y1, X2, y2, loading.mat, model="linear", intercept.loading=TRUE, verbose=TRUE)
#> Call: Inference for Linear Functional ======> Data 1/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3 
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 4
```

`ci` method for `ITE`

``` r
ci(Est)
#>   loading      lower      upper
#> 1       1 -0.9387936  0.0701035
#> 2       2 -1.4255105 -0.5516337
```

`summary` method for `ITE`

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     -0.467    -0.4343     0.2574  -1.688 9.149e-02   .
#>        2     -1.026    -0.9886     0.2229  -4.434 9.232e-06 ***
```

### Individualized Treatment Effect in logistic regression model

Generate Data and find the truth linear functionals:

``` r
## 1st data
X1 = matrix(rnorm(100*120), nrow=100, ncol=120)
exp_val1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1
prob1 = exp(exp_val1) / (1 + exp(exp_val1))
y1 = rbinom(100, 1, prob1)
## 2nd data
X2 = matrix(0.8*rnorm(100*120), nrow=100, ncol=120)
exp_val2 = -0.5 + X2[,1] * 0.8 + X2[,2] * 0.8
prob2 = exp(exp_val2) / (1 + exp(exp_val2))
y2 = rbinom(100, 1, prob2)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=TRUE
truth1 = (-0.5 + 0.5*1 + 1*1) - (-0.5 + 0.8*1 + 0.8*1)
truth2 = (-0.5 + 0.5*(-0.5) + 1*(-1)) - (-0.5 + 0.8*(-0.5) + 0.8*(-1))
truth = c(truth1, truth2)
prob.fun = function(x) exp(x)/(1+exp(x))
truth.prob1 = prob.fun(-0.5 + 0.5*1 + 1*1) - prob.fun(-0.5 + 0.8*1 + 0.8*1)
truth.prob2 = prob.fun(-0.5 + 0.5*(-0.5) + 1*(-1)) - prob.fun(-0.5 + 0.8*(-0.5) + 0.8*(-1))
truth.prob = c(truth.prob1, truth.prob2)

truth; truth.prob
#> [1] -0.10 -0.05
#> [1] -0.019201527 -0.006418067
```

Call `ITE` with `model="logistic"` or `model="logisitc_alternative"`:

``` r
Est = ITE(X1, y1, X2, y2, loading.mat, model="logistic", intercept.loading=TRUE, verbose = TRUE)
#> Call: Inference for Linear Functional ======> Data 1/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3 
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 4
```

`ci` method for `ITE`:

``` r
## confidence interval for linear combination
ci(Est)
#>   loading     lower     upper
#> 1       1 -1.400813 0.9062710
#> 2       2 -1.658636 0.5038927
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading      lower      upper
#> 1       1 -0.3213497 0.20759032
#> 2       2 -0.2267113 0.07364488
```

### Quadratic functional in linear regression

Generate Data and find the truth quadratic functionals:

``` r
A1gen <- function(rho, p){
  M = matrix(NA, nrow=p, ncol=p)
  for(i in 1:p) for(j in 1:p) M[i,j] = rho^{abs(i-j)}
  M
}
Cov = A1gen(0.5, 150)
X = MASS::mvrnorm(n=200, mu=rep(0, 150), Sigma=Cov)
beta = rep(0, 150); beta[25:50] = 0.2
y = X%*%beta + rnorm(200)
test.set = c(40:60)
truth = as.numeric(t(beta[test.set])%*%Cov[test.set, test.set]%*%beta[test.set])
truth
#> [1] 1.160078
```

Call `QF` with `model="linear"`:

``` r
tau.vec = c(0, 0.5, 1)
Est = QF(X, y, G=test.set, A=NULL, model="linear", tau.vec=tau.vec, verbose=TRUE)
#> Computing QF... 
#> ---> Direction is identified at step: 5
Est$est.plugin ## plugin(biased) estimator
#> [1] 0.8375219
Est$est.debias ## bias-corrected estimator
#> [1] 1.170038
Est$se.vec ## standard errors for bias-corrected estimator
#> [1] 0.1492490 0.1574016 0.1651523
Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#>            lower    upper
#> tau0   0.8775150 1.462561
#> tau0.5 0.8615362 1.478539
#> tau1   0.8463452 1.493730
```

`ci` method for `QF`

``` r
ci(Est)
#>   tau     lower    upper
#> 1 0.0 0.8775150 1.462561
#> 2 0.5 0.8615362 1.478539
#> 3 1.0 0.8463452 1.493730
```

`summary` method for `QF`

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0     0.8375       1.17     0.1492   7.839 4.441e-15 ***
#>  0.5     0.8375       1.17     0.1574   7.433 1.057e-13 ***
#>  1.0     0.8375       1.17     0.1652   7.085 1.394e-12 ***
```

### Quadratic functional in logistic regression

Generate Data and find the truth quadratic functional

``` r
X = MASS::mvrnorm(n=200, mu=rep(0, 150), Sigma=Cov)
beta = rep(0, 150); beta[25:50] = 0.2
exp_val = X%*%beta
prob = exp(exp_val) / (1+exp(exp_val))
y = rbinom(200, 1, prob)
test.set = c(40:60)
truth = as.numeric(t(beta[test.set]%*%Cov[test.set, test.set]%*%beta[test.set]))
truth
#> [1] 1.160078
```

Call `QF` with `model="logistic"` or `model="logisitc_alternative"`:

``` r
tau.vec = c(0, 0.5, 1)
Est = QF(X, y, G=test.set, A=NULL, model="logistic", tau.vec = tau.vec, verbose=TRUE)
#> Computing QF... 
#> ---> Direction is identified at step: 5
```

`ci` method for `QF`:

``` r
ci(Est)
#>   tau     lower     upper
#> 1 0.0 0.2141498 0.6366166
#> 2 0.5 0.1925245 0.6582420
#> 3 1.0 0.1727435 0.6780230
```

`summary` method for `QF`:

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0     0.1218     0.4254     0.1078   3.947 7.914e-05 ***
#>  0.5     0.1218     0.4254     0.1188   3.580 3.430e-04 ***
#>  1.0     0.1218     0.4254     0.1289   3.300 9.665e-04 ***
```

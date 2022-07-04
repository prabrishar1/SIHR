
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

## Example

These are basic examples which show how to solve the common
high-dimensional inference problems:

``` r
library(SIHR)
```

### Inference for linear functional in high-dimensional linear regression model

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
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3
Est$est.plugin.vec ## plugin(biased) estimators
#> [1]  0.559069 -1.613608
Est$est.debias.vec ## bias-corrected estimators
#> [1]  0.8091347 -1.7912063
Est$se.vec ## standard errors for bias-corrected estimators
#> [1] 0.1669583 0.1444781
Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#>               lower     upper
#> loading1  0.4819024  1.136367
#> loading2 -2.0743781 -1.508034
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

### Inference for linear functional in high-dimensional logistic regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
X = matrix(rnorm(100*120), nrow=100, ncol=120)
exp_val = -0.5 + X[,1] * 0.5 + X[,2] * 1
prob = exp(exp_val) / (1+exp(exp_val))
y = rbinom(100, 1, prob)
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

Call `LF` with `model="logistic"` or `model=logistic_alternative"`:

``` r
## model = "logisitc" or "logistic_alternative"
Est = LF(X, y, loading.mat, model="logistic", intercept.loading=TRUE, verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3
Est$est.plugin.vec ## plugin(biased) estimators
#> [1]  0.5213585 -1.1018881
Est$est.debias.vec ## bias-corrected estimators
#> [1]  0.8592612 -1.3480456
Est$se.vec ## standard errors for bias-corrected estimators
#> [1] 0.4590642 0.4015254
Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#>                lower      upper
#> loading1 -0.04048804  1.7590104
#> loading2 -2.13502097 -0.5610702
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading       lower      upper
#> 1       1 -0.04048804  1.7590104
#> 2       2 -2.13502097 -0.5610702
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)    
#>        1     0.5214     0.8593     0.4591   1.872 0.061239   .
#>        2    -1.1019    -1.3480     0.4015  -3.357 0.000787 ***
```

### Inference for Treatment Effects in high-dimensional linear regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
X1 = matrix(rnorm(100*120), nrow=100, ncol=120)
y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
X2 = matrix(0.8*rnorm(100*120), nrow=100, ncol=120)
y2 = 0.1 + X2[,1] * 0.8 + X2[,2] * 0.8 + rnorm(100)
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
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
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> ---> Initial step set as: 3 
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Initial step set as: 4 
#> ---> Finding Direction with step: 4 
#> ---> Direction is identified at step: 4
Est$est.plugin.vec ## plugin(biased) estimators
#> [1] -0.6267262 -0.8462001
Est$est.debias.vec ## bias-corrected estimators
#> [1] -0.6448568 -0.7051121
Est$se.vec ## standard errors for bias-corrected estimators
#> [1] 0.2544411 0.2304547
Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#>              lower      upper
#> loading1 -1.143552 -0.1461613
#> loading2 -1.156795 -0.2534291
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading     lower      upper
#> 1       1 -1.143552 -0.1461613
#> 2       2 -1.156795 -0.2534291
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)   
#>        1    -0.6267    -0.6449     0.2544  -2.534 0.011264  *
#>        2    -0.8462    -0.7051     0.2305  -3.060 0.002216 **
```

### Inference for Quadratic functional in high-dimensional linear regression

Generate Data and find the truth quadratic functionals:

``` r
set.seed(0)
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
#> ---> Initial step set as: 5 
#> ---> Finding Direction with step: 5 
#> ---> Direction is identified at step: 5
Est$est.plugin ## plugin(biased) estimator
#> [1] 1.119486
Est$est.debias ## bias-corrected estimator
#> [1] 1.41201
Est$se.vec ## standard errors for bias-corrected estimator
#> [1] 0.1665746 0.1739169 0.1809616
Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#>           lower    upper
#> tau0   1.085530 1.738490
#> tau0.5 1.071139 1.752881
#> tau1   1.057332 1.766688
```

`ci` method for `LF`

``` r
ci(Est)
#>   tau    lower    upper
#> 1 0.0 1.085530 1.738490
#> 2 0.5 1.071139 1.752881
#> 3 1.0 1.057332 1.766688
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0      1.119      1.412     0.1666   8.477 0.000e+00 ***
#>  0.5      1.119      1.412     0.1739   8.119 4.441e-16 ***
#>  1.0      1.119      1.412     0.1810   7.803 5.995e-15 ***
```

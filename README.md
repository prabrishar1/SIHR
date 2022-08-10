
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

### Linear functional in linear regression model - 1

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
X = matrix(rnorm(100*120), nrow=100, ncol=120)
y = -0.5 + X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=FALSE
truth1 = 0.5 * 1 + 1 * 1
truth2 = 0.5 * -0.5 + 1 * -1
truth = c(truth1, truth2)
truth
#> [1]  1.50 -1.25
```

In the example, the linear functional does not involve the intercept
term, so we set `intercept.loading=FALSE` (default). If users want to
include the intercept term, please set `intercept.loading=TRUE`, such
that truth1 = -0.5 + 1.5 = 1; truth2 = -0.5 - 1.25 = -1.75

Call `LF` with `model="linear"`:

``` r
Est = LF(X, y, loading.mat, model="linear", intercept=TRUE, intercept.loading=FALSE, verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading     lower     upper
#> 1       1  1.173659  1.602305
#> 2       2 -1.371465 -1.035460
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)    
#>        1      1.158      1.388    0.10935   12.69 0.00e+00 ***
#>        2     -1.015     -1.203    0.08572  -14.04 8.88e-45 ***
```

### Linear functional in linear regression model - 2

Sometimes, we may be interested in multiple linear functionals, each
with a separate loading. To be computationally efficient, we can specify
the argument `beta.init` first, so that the program can save time to
compute the initial estimator repeatedly.

``` r
set.seed(1)
X = matrix(rnorm(100*120), nrow=100, ncol=120)
y = -0.5 + X[,1:10] %*% rep(0.5, 10) + rnorm(100)
loading.mat = matrix(0, nrow=120, ncol=10)
for(i in 1:ncol(loading.mat)){
  loading.mat[i,i] =  1
}
```

``` r
library(glmnet)
#> Loading required package: Matrix
#> Loaded glmnet 4.1-4
cvfit = cv.glmnet(X, y, family = "gaussian", alpha = 1, intercept = TRUE, standardize = T)
beta.init = as.vector(coef(cvfit, s = cvfit$lambda.min))
```

Call `LF` with `model="linear"`:

``` r
Est = LF(X, y, loading.mat, model="linear", intercept=TRUE, beta.init=beta.init, verbose=TRUE)
#> Computing LF for loading (1/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (3/10)... 
#> ---> Direction is identified at step: 4 
#> Computing LF for loading (4/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (5/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (6/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (7/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (8/10)... 
#> ---> Direction is identified at step: 4 
#> Computing LF for loading (9/10)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (10/10)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `LF`

``` r
ci(Est)
#>    loading     lower     upper
#> 1        1 0.1837947 0.5536739
#> 2        2 0.3506567 0.7000917
#> 3        3 0.3380023 0.7160656
#> 4        4 0.2034572 0.5375176
#> 5        5 0.3234705 0.6049655
#> 6        6 0.2150711 0.5599059
#> 7        7 0.3222297 0.6268434
#> 8        8 0.2275796 0.5680806
#> 9        9 0.5282958 0.8399946
#> 10      10 0.2308572 0.5459160
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.2698     0.3687    0.09436   3.908 9.314e-05 ***
#>        2     0.4145     0.5254    0.08914   5.894 3.779e-09 ***
#>        3     0.4057     0.5270    0.09645   5.465 4.642e-08 ***
#>        4     0.2631     0.3705    0.08522   4.347 1.378e-05 ***
#>        5     0.3773     0.4642    0.07181   6.464 1.017e-10 ***
#>        6     0.2730     0.3875    0.08797   4.405 1.059e-05 ***
#>        7     0.3664     0.4745    0.07771   6.107 1.018e-09 ***
#>        8     0.2911     0.3978    0.08686   4.580 4.652e-06 ***
#>        9     0.5699     0.6841    0.07952   8.604 0.000e+00 ***
#>       10     0.2839     0.3884    0.08037   4.832 1.350e-06 ***
```

### Linear functional in logistic regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
X = matrix(rnorm(100*120), nrow=100, ncol=120)
exp_val = -0.5 + X[,1] * 0.5 + X[,2] * 1
prob = exp(exp_val) / (1+exp(exp_val))
y = rbinom(100, 1, prob)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
## consider the intercept.loading=TRUE
truth1 = 0.5 * 1 + 1 * 1
truth2 = 0.5 * -0.5 + 1 * -1
truth = c(truth1, truth2)
truth.prob = exp(truth) / (1 + exp(truth))
truth; truth.prob
#> [1]  1.50 -1.25
#> [1] 0.8175745 0.2227001
```

Call `LF` with `model="logistic"` or `model="logistic_alternative"`:

``` r
## model = "logisitc" or "logistic_alternative"
Est = LF(X, y, loading.mat, model="logistic", verbose=TRUE)
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
#> 1       1  0.5806516  1.8170370
#> 2       2 -1.5701698 -0.5695987
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.6412173 0.8602102
#> 2       2 0.1721922 0.3613294
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.8116      1.199     0.3154   3.801 1.442e-04 ***
#>        2    -0.8116     -1.070     0.2553  -4.191 2.771e-05 ***
```

### Individualized Treatment Effect in linear regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
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
truth1 = (0.5*1 + 1*1) - (0.8*1 + 0.8*1)
truth2 = (0.5*(-0.5) + 1*(-1)) - (0.8*(-0.5) + 0.8*(-1))
truth = c(truth1, truth2)
truth
#> [1] -0.10 -0.05
```

Call `ITE` with `model="linear"`:

``` r
Est = ITE(X1, y1, X2, y2, loading.mat, model="linear", verbose=TRUE)
#> Call: Inference for Linear Functional ======> Data 1/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3 
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `ITE`

``` r
ci(Est)
#>   loading      lower     upper
#> 1       1 -0.4902385 0.2530740
#> 2       2 -0.3724087 0.2332782
```

`summary` method for `ITE`

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)  
#>        1    0.02711   -0.11858     0.1896 -0.6254   0.5317  
#>        2   -0.19236   -0.06957     0.1545 -0.4502   0.6526
```

### Individualized Treatment Effect in logistic regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
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
truth1 = (0.5*1 + 1*1) - (0.8*1 + 0.8*1)
truth2 = (0.5*(-0.5) + 1*(-1)) - (0.8*(-0.5) + 0.8*(-1))
truth = c(truth1, truth2)
prob.fun = function(x) exp(x)/(1+exp(x))
truth.prob1 = prob.fun(0.5*1 + 1*1) - prob.fun(0.8*1 + 0.8*1)
truth.prob2 = prob.fun(0.5*(-0.5) + 1*(-1)) - prob.fun(0.8*(-0.5) + 0.8*(-1))
truth.prob = c(truth.prob1, truth.prob2)

truth; truth.prob
#> [1] -0.10 -0.05
#> [1] -0.014443909 -0.008775078
```

Call `ITE` with `model="logistic"` or `model="logisitc_alternative"`:

``` r
Est = ITE(X1, y1, X2, y2, loading.mat, model="logistic", verbose = TRUE)
#> Call: Inference for Linear Functional ======> Data 1/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3 
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
#> ---> Direction is identified at step: 3
```

`ci` method for `ITE`:

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower     upper
#> 1       1 -0.7585971 1.0153884
#> 2       2 -1.1008198 0.3515386
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading      lower      upper
#> 1       1 -0.1395789 0.18680545
#> 2       2 -0.2272414 0.07275771
```

`summary` method for `ITE`:

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)  
#>        1     0.5292     0.1284     0.4526  0.2837   0.7766  
#>        2    -0.6787    -0.3746     0.3705 -1.0112   0.3119
```

### Quadratic functional in linear regression

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
#> ---> Direction is identified at step: 5
```

`ci` method for `QF`

``` r
ci(Est)
#>   tau    lower    upper
#> 1 0.0 1.085530 1.738490
#> 2 0.5 1.071139 1.752881
#> 3 1.0 1.057332 1.766688
```

`summary` method for `QF`

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

### Quadratic functional in logistic regression

Generate Data and find the truth quadratic functional

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
#>   tau     lower    upper
#> 1 0.0 0.7408816 1.983207
#> 2 0.5 0.7331988 1.990890
#> 3 1.0 0.7256086 1.998480
```

`summary` method for `QF`:

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0     0.7146      1.362     0.3169   4.298 1.726e-05 ***
#>  0.5     0.7146      1.362     0.3208   4.245 2.184e-05 ***
#>  1.0     0.7146      1.362     0.3247   4.195 2.734e-05 ***
```

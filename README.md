
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
#> The projection direction is identified at mu = 0.061329
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading     lower     upper
#> 1       1  1.139676  1.694804
#> 2       2 -1.506535 -1.043502
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1      1.158      1.417     0.1416   10.01 0.000e+00 ***
#>        2     -1.015     -1.275     0.1181  -10.79 3.674e-27 ***
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
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (2/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (3/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (4/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (5/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (6/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (7/10)... 
#> The projection direction is identified at mu = 0.027257
#> Computing LF for loading (8/10)... 
#> The projection direction is identified at mu = 0.027257
#> Computing LF for loading (9/10)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (10/10)... 
#> The projection direction is identified at mu = 0.040886
```

`ci` method for `LF`

``` r
ci(Est)
#>    loading       lower     upper
#> 1        1  0.04983623 0.7442021
#> 2        2  0.22550997 1.1867197
#> 3        3  0.18050861 0.8728558
#> 4        4  0.08456954 0.7045124
#> 5        5  0.21977022 0.9906651
#> 6        6 -0.25811918 0.6589827
#> 7        7  0.36302046 0.9668954
#> 8        8  0.04997011 0.7365193
#> 9        9  0.48755974 1.0346292
#> 10      10  0.14951052 0.7344981
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.2698     0.3970     0.1771  2.2413 2.501e-02   *
#>        2     0.4145     0.7061     0.2452  2.8796 3.982e-03  **
#>        3     0.4057     0.5267     0.1766  2.9820 2.864e-03  **
#>        4     0.2631     0.3945     0.1582  2.4947 1.261e-02   *
#>        5     0.3773     0.6052     0.1967  3.0775 2.088e-03  **
#>        6     0.2730     0.2004     0.2340  0.8567 3.916e-01    
#>        7     0.3664     0.6650     0.1541  4.3164 1.586e-05 ***
#>        8     0.2911     0.3932     0.1751  2.2453 2.475e-02   *
#>        9     0.5699     0.7611     0.1396  5.4535 4.939e-08 ***
#>       10     0.2839     0.4420     0.1492  2.9618 3.058e-03  **
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
#> The projection direction is identified at mu = 0.061329
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329
```

`ci` method for `LF`

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower      upper
#> 1       1  0.7134236  2.1958503
#> 2       2 -1.9003228 -0.6180052
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.6711572 0.8998762
#> 2       2 0.1300719 0.3502353
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.8116      1.455     0.3782   3.846 0.0001198 ***
#>        2    -0.8116     -1.259     0.3271  -3.849 0.0001185 ***
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
#> The projection direction is identified at mu = 0.061329
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.027257
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.040886
```

`ci` method for `ITE`

``` r
ci(Est)
#>   loading      lower     upper
#> 1       1 -0.9634614 0.6422924
#> 2       2 -0.5728741 0.4081881
```

`summary` method for `ITE`

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)  
#>        1   -0.03658   -0.16058     0.4096  -0.392   0.6950  
#>        2   -0.14526   -0.08234     0.2503  -0.329   0.7421
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
#> The projection direction is identified at mu = 0.061329
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.040886
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.040886
```

`ci` method for `ITE`:

``` r
## confidence interval for linear combination
ci(Est)
#>   loading     lower     upper
#> 1       1 -1.463932 1.7455369
#> 2       2 -1.841519 0.8706693
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading      lower     upper
#> 1       1 -0.2378692 0.2826141
#> 2       2 -0.3697559 0.1819994
```

`summary` method for `ITE`:

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)  
#>        1     0.5027     0.1408     0.8188  0.1720   0.8635  
#>        2    -0.6654    -0.4854     0.6919 -0.7016   0.4829
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
#> The projection direction is identified at mu=0.013143
```

`ci` method for `QF`

``` r
ci(Est)
#>   tau    lower    upper
#> 1 0.0 1.046298 1.888180
#> 2 0.5 1.035042 1.899436
#> 3 1.0 1.024071 1.910407
```

`summary` method for `QF`

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0      1.119      1.467     0.2148   6.832 8.392e-12 ***
#>  0.5      1.119      1.467     0.2205   6.654 2.857e-11 ***
#>  1.0      1.119      1.467     0.2261   6.489 8.639e-11 ***
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
#> The projection direction is identified at mu=0.019714
```

`ci` method for `QF`:

``` r
ci(Est)
#>   tau     lower    upper
#> 1 0.0 0.7642630 2.204158
#> 2 0.5 0.7576239 2.210797
#> 3 1.0 0.7510450 2.217376
```

`summary` method for `QF`:

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>  tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.0     0.7146      1.484     0.3673   4.041 5.332e-05 ***
#>  0.5     0.7146      1.484     0.3707   4.004 6.237e-05 ***
#>  1.0     0.7146      1.484     0.3741   3.968 7.256e-05 ***
```

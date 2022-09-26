
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
“logistic_alter” or “probit”.

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
#> The projection direction is identified at mu = 0.061329at step =3
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
```

`ci` method for `LF`

``` r
ci(Est)
#>   loading     lower     upper
#> 1       1  1.111919  1.722561
#> 2       2 -1.529687 -1.020350
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1      1.158      1.417     0.1558   9.098 0.000e+00 ***
#>        2     -1.015     -1.275     0.1299  -9.813 9.924e-23 ***
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
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (2/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (3/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (4/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (5/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (6/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (7/10)... 
#> The projection direction is identified at mu = 0.027257at step =5
#> Computing LF for loading (8/10)... 
#> The projection direction is identified at mu = 0.027257at step =5
#> Computing LF for loading (9/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (10/10)... 
#> The projection direction is identified at mu = 0.040886at step =4
```

`ci` method for `LF`

``` r
ci(Est)
#>    loading       lower     upper
#> 1        1  0.01511794 0.7789204
#> 2        2  0.17744949 1.2347802
#> 3        3  0.14589125 0.9074732
#> 4        4  0.05357240 0.7355096
#> 5        5  0.18122547 1.0292098
#> 6        6 -0.30397428 0.7048378
#> 7        7  0.33282671 0.9970891
#> 8        8  0.01564265 0.7708467
#> 9        9  0.46020627 1.0619827
#> 10      10  0.12026114 0.7637474
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.2698     0.3970     0.1949  2.0376 4.159e-02   *
#>        2     0.4145     0.7061     0.2697  2.6178 8.849e-03  **
#>        3     0.4057     0.5267     0.1943  2.7109 6.711e-03  **
#>        4     0.2631     0.3945     0.1740  2.2679 2.333e-02   *
#>        5     0.3773     0.6052     0.2163  2.7977 5.147e-03  **
#>        6     0.2730     0.2004     0.2574  0.7788 4.361e-01    
#>        7     0.3664     0.6650     0.1695  3.9240 8.708e-05 ***
#>        8     0.2911     0.3932     0.1927  2.0412 4.124e-02   *
#>        9     0.5699     0.7611     0.1535  4.9577 7.133e-07 ***
#>       10     0.2839     0.4420     0.1642  2.6926 7.091e-03  **
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

Call `LF` with `model="logistic"` or `model="logistic_alter"`:

``` r
## model = "logisitc"
Est = LF(X, y, loading.mat, model="logistic", verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
```

`ci` method for `LF`

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower      upper
#> 1       1  0.6393023  2.2699716
#> 2       2 -1.9644387 -0.5538893
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.6545957 0.9063594
#> 2       2 0.1229875 0.3649625
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.8116      1.455     0.4160   3.497 0.0004709 ***
#>        2    -0.8116     -1.259     0.3598  -3.499 0.0004666 ***
```

Call `LF` with `model="logistic_alter"`:

``` r
## model = "logistic_alter"
Est = LF(X, y, loading.mat, model="logistic_alter", verbose=TRUE)
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
```

`ci` method for `LF`

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower      upper
#> 1       1  0.6121908  2.1603820
#> 2       2 -1.8474229 -0.5201972
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.6484404 0.8966350
#> 2       2 0.1361758 0.3728061
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.7597      1.386     0.3950   3.510 0.0004481 ***
#>        2    -0.7597     -1.184     0.3386  -3.496 0.0004717 ***
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
#> The projection direction is identified at mu = 0.061329at step =3
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.027257at step =5
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.040886at step =4
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

Call `ITE` with `model="logistic"` or `model="logisitc_alter"`:

``` r
Est = ITE(X1, y1, X2, y2, loading.mat, model="logistic", verbose = TRUE)
#> Call: Inference for Linear Functional ======> Data 1/2 
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.061329at step =3
#> Call: Inference for Linear Functional ======> Data 2/2 
#> Computing LF for loading (1/2)... 
#> The projection direction is identified at mu = 0.040886at step =4
#> Computing LF for loading (2/2)... 
#> The projection direction is identified at mu = 0.040886at step =4
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
Cov = A1gen(0.5, 150)/2
X = MASS::mvrnorm(n=400, mu=rep(0, 150), Sigma=Cov)
beta = rep(0, 150); beta[25:50] = 0.2
y = X%*%beta + rnorm(400)
test.set = c(40:60)
truth = as.numeric(t(beta[test.set])%*%Cov[test.set, test.set]%*%beta[test.set])
truth
#> [1] 0.5800391
```

Call `QF` with `model="linear"` with intial estimator given:

``` r
library(glmnet)
outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                            intercept = T, standardize = T)
beta.init = as.vector(coef(outLas, s = outLas$lambda.min))
tau = c(0.25, 0.5)
Est = QF(X, y, G=test.set, A=NULL, model="linear", beta.init=beta.init, tau=tau, verbose=TRUE)
#> Computing QF... 
#> The projection direction is identified at mu = 0.01394at step =6
```

`ci` method for `QF`

``` r
ci(Est)
#>    tau     lower     upper
#> 1 0.25 0.4442377 0.7842335
#> 2 0.50 0.4197381 0.8087330
```

`summary` method for `QF`

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>   tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.25     0.4958     0.6142    0.08674   7.082 1.424e-12 ***
#>  0.50     0.4958     0.6142    0.09924   6.190 6.028e-10 ***
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
Cov = A1gen(0.5, 150)/2
X = MASS::mvrnorm(n=400, mu=rep(0, 150), Sigma=Cov)
beta = rep(0, 150); beta[25:50] = 0.2
exp_val = X%*%beta
prob = exp(exp_val) / (1+exp(exp_val))
y = rbinom(400, 1, prob)
test.set = c(40:60)
truth = as.numeric(t(beta[test.set]%*%Cov[test.set, test.set]%*%beta[test.set]))
truth
#> [1] 0.5800391
```

Call `QF` with `model="logistic"` or `model="logisitc"`:

``` r
tau = c(0.25, 0.5)
Est = QF(X, y, G=test.set, A=NULL, model="logistic", split=T, tau=tau, verbose=TRUE)
#> Computing QF... 
#> The projection direction is identified at mu = 0.013143at step =7
```

`ci` method for `QF`:

``` r
ci(Est)
#>    tau lower     upper
#> 1 0.25     0 0.9120369
#> 2 0.50     0 1.2316317
```

`summary` method for `QF`:

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>   tau est.plugin est.debias Std. Error z value Pr(>|z|)  
#>  0.25     0.2174     0.2059     0.3603  0.5714   0.5677  
#>  0.50     0.2174     0.2059     0.5234  0.3934   0.6941
```

Call `QF` with `model="logisitc_alter"`:

``` r
tau = c(0.25, 0.5)
Est = QF(X, y, G=test.set, A=NULL, model="logistic_alter", split=T, tau=tau, verbose=TRUE)
#> Computing QF... 
#> The projection direction is identified at mu = 0.013143at step =7
```

`ci` method for `QF`:

``` r
ci(Est)
#>    tau lower    upper
#> 1 0.25     0 1.120806
#> 2 0.50     0 1.563322
```

`summary` method for `QF`:

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>   tau est.plugin est.debias Std. Error z value Pr(>|z|)  
#>  0.25      0.191     0.2804     0.4288  0.6540   0.5131  
#>  0.50      0.191     0.2804     0.6546  0.4284   0.6684
```

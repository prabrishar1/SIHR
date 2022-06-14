
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals in generalized
linear regression (GLM_LF), (2) quadratic functionals in linear
regression (QF) (3) individual treatment effects in generalized linear
regression (ITE). \## Installation

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
loading.mat = cbind(c(1, 1, rep(0, 118)), c(-0.5, -1, rep(0, 118)))
## consider the intercept.loading 
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
#> ---> Finding Direction with step: 3 
#> ---> Direction is identified at step: 3 
#> Computing LF for loading (2/2)... 
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
#> 
#> Confidence Intervals:
#>  loading   lower  upper
#>        1  0.4819  1.136
#>        2 -2.0744 -1.508
```

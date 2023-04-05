
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals in generalized
linear regression, (2) conditional average treatment effects in generalized
linear regression (CATE), (3) quadratic functionals in generalized
linear regression (QF) (4) inner product in generalized linear
regression (InnProd) and (5) distance in generalized linear regression
(Dist).

Currently, we support different generalized linear regression, by
specifying the argument `model` in “linear”, “logisitc”,
“logistic_alter”.

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

Note that both true values are included in their corresponding
confidence intervals.

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

`summary()` function returns the summary statistics, including the
plugin estimator, the bias-corrected estimator, standard errors.

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
Est = LF(X, y, loading.mat, model="linear", intercept=TRUE, beta.init=beta.init, verbose=FALSE)
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
#>   loading      lower     upper
#> 1       1  0.6077181  2.191417
#> 2       2 -1.8922856 -0.530151
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading     lower     upper
#> 1       1 0.6474201 0.8994761
#> 2       2 0.1309841 0.3704817
```

`summary` method for `LF`

``` r
summary(Est)
#> Call: 
#> Inference for Linear Functional
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1     0.7942      1.400     0.4040   3.464 0.0005319 ***
#>        2    -0.7942     -1.211     0.3475  -3.486 0.0004910 ***
```

### Conditional Average Treatment Effect in linear regression model

Generate Data and find the truth linear functionals:

``` r
set.seed(0)
## 1st data
X1 = matrix(rnorm(100*120), nrow=100, ncol=120)
y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
## 2nd data
X2 = matrix(0.8*rnorm(100*120), nrow=100, ncol=120)
y2 = 0.1 + X2[,1] * 1.8 + X2[,2] * 1.8 + rnorm(100)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
truth1 = (1.8*1 + 1.8*1) - (0.5*1 + 1*1)
truth2 = (1.8*(-0.5) + 1.8*(-1))- (0.5*(-0.5) + 1*(-1))
truth = c(truth1, truth2)
truth
#> [1]  2.10 -1.45
```

Call `CATE` with `model="linear"`:

``` r
Est = CATE(X1, y1, X2, y2, loading.mat, model="linear")
```

`ci` method for `CATE`

``` r
ci(Est)
#>   loading     lower      upper
#> 1       1  1.338908  2.9843155
#> 2       2 -1.931858 -0.9300702
```

`summary` method for `CATE`

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>        1      1.991      2.162     0.4198   5.150 2.609e-07 ***
#>        2     -1.321     -1.431     0.2556  -5.599 2.153e-08 ***
```

### Conditional Average Treatment Effect in logistic regression model

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
exp_val2 = -0.5 + X2[,1] * 1.8 + X2[,2] * 1.8
prob2 = exp(exp_val2) / (1 + exp(exp_val2))
y2 = rbinom(100, 1, prob2)
## loadings
loading1 = c(1, 1, rep(0, 118))
loading2 = c(-0.5, -1, rep(0, 118))
loading.mat = cbind(loading1, loading2)
truth1 = (1.8*1 + 1.8*1) - (0.5*1 + 1*1)
truth2 = (0.8*(-0.5) + 0.8*(-1)) - (0.5*(-0.5) + 1*(-1)) 
truth = c(truth1, truth2)
prob.fun = function(x) exp(x)/(1+exp(x))
truth.prob1 = prob.fun(1.8*1 + 1.8*1) - prob.fun(0.5*1 + 1*1)
truth.prob2 = prob.fun(1.8*(-0.5) + 1.8*(-1)) - prob.fun(0.5*(-0.5) + 1*(-1)) 
truth.prob = c(truth.prob1, truth.prob2)

truth; truth.prob
#> [1] 2.10 0.05
#> [1]  0.1558285 -0.1597268
```

Call `CATE` with `model="logistic"` or `model="logisitc_alter"`:

``` r
Est = CATE(X1, y1, X2, y2, loading.mat, model="logistic", verbose = FALSE)
```

`ci` method for `CATE`:

``` r
## confidence interval for linear combination
ci(Est)
#>   loading      lower    upper
#> 1       1 -0.4253334 3.397421
#> 2       2 -3.3720814 1.259623
## confidence interval after probability transformation
ci(Est, probability = TRUE)
#>   loading       lower      upper
#> 1       1 -0.01213062 0.28746833
#> 2       2 -0.34578964 0.08603778
```

`summary` method for `CATE`:

``` r
summary(Est)
#> Call: 
#> Inference for Treatment Effect
#> 
#> Estimators: 
#>  loading est.plugin est.debias Std. Error z value Pr(>|z|)  
#>        1     0.9234      1.486     0.9752  1.5238   0.1276  
#>        2    -0.3643     -1.056     1.1816 -0.8939   0.3714
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
Est = QF(X, y, G=test.set, A=NULL, model="linear", beta.init=beta.init, verbose=FALSE)
```

`ci` method for `QF`

``` r
ci(Est)
#>    tau     lower     upper
#> 1 0.25 0.4397755 0.7416457
#> 2 0.50 0.4320213 0.7494000
#> 3 1.00 0.4175514 0.7638699
```

`summary` method for `QF`

``` r
summary(Est)
#> Call: 
#> Inference for Quadratic Functional
#> 
#>   tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.25     0.4547     0.5907    0.07701   7.671 1.710e-14 ***
#>  0.50     0.4547     0.5907    0.08097   7.296 2.969e-13 ***
#>  1.00     0.4547     0.5907    0.08835   6.686 2.291e-11 ***
```

### Inner product in linear regression model

Generate Data and find the true inner product:

``` r
set.seed(0)
p = 120
mu = rep(0,p)
Cov = diag(p)
## 1st data
n1 = 200
X1 = MASS::mvrnorm(n1,mu,Cov)
beta1 = rep(0, p); beta1[c(1,2)] = c(0.5, 1)
y1 = X1%*%beta1 + rnorm(n1)
## 2nd data
n2 = 200
X2 = MASS::mvrnorm(n2,mu,Cov)
beta2 = rep(0, p); beta2[c(1,2)] = c(1.8, 0.8)
y2 = X2%*%beta2 + rnorm(n2)
## test.set
G =c(1:10)

truth <- as.numeric(t(beta1[G])%*%Cov[G,G]%*%beta2[G])
truth
#> [1] 1.7
```

Call `InnProd` with `model="linear"`:

``` r
Est = InnProd(X1, y1, X2, y2, G, model="linear")
```

`ci` method for `InnProd`

``` r
ci(Est)
#>    tau     lower    upper
#> 1 0.25 0.8118224 2.376767
#> 2 0.50 0.7628233 2.425767
#> 3 1.00 0.6648251 2.523765
```

`summary` method for `InnProd`

``` r
summary(Est)
#> Call: 
#> Inference for Inner Product
#> 
#>   tau est.plugin est.debias Std. Error z value  Pr(>|z|)    
#>  0.25     0.9745      1.594     0.3992   3.993 6.512e-05 ***
#>  0.50     0.9745      1.594     0.4242   3.758 1.712e-04 ***
#>  1.00     0.9745      1.594     0.4742   3.362 7.742e-04 ***
```

### Distance in linear regression model

Generate Data and find the true distance:

``` r
set.seed(0)
p = 120
mu = rep(0,p)
Cov = diag(p)
## 1st data
n1 = 200
X1 = MASS::mvrnorm(n1,mu,Cov)
beta1 = rep(0, p); beta1[c(1,2)] = c(0.5, 1)
y1 = X1%*%beta1 + rnorm(n1)
## 2nd data
n2 = 200
X2 = MASS::mvrnorm(n2,mu,Cov)
beta2 = rep(0, p); beta2[c(1,2)] = c(1.8, 1.8)
y2 = X2%*%beta2 + rnorm(n2)
## test.set
G =c(1:10)

truth <- as.numeric(t(beta1[G]-beta2[G])%*%(beta1[G]-beta2[G]))
truth
#> [1] 2.33
```

Call `Dist` with `model="linear"`:

``` r
Est = Dist(X1, y1, X2, y2, G, model="linear", A = diag(length(G)))
```

`ci` method for `Dist`

``` r
ci(Est)
#>    tau     lower    upper
#> 1 0.25 0.7528571 3.721829
#> 2 0.50 0.7038580 3.770828
#> 3 1.00 0.6058598 3.868826
```

`summary` method for `Dist`

``` r
summary(Est)
#> Call: 
#> Inference for Distance
#> 
#>   tau est.plugin est.debias Std. Error z value Pr(>|z|)   
#>  0.25      1.716      2.237     0.7574   2.954 0.003137 **
#>  0.50      1.716      2.237     0.7824   2.860 0.004242 **
#>  1.00      1.716      2.237     0.8324   2.688 0.007192 **
```

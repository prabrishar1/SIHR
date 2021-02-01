
<!-- README.md is generated from README.Rmd. Please edit that file -->

FIHR
====

<!-- badges: start -->
<!-- badges: end -->

The goal of FIHR is to provide inference for linear and quadratic
functionals in high-dimensional linear and logistic regression models.
It computes bias-corrected estimators and corresponding standard errors
for the linear and quadratic functionals.

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("prabrishar1/FIHR")

Example
-------

These are basic examples which show how to solve the common
high-dimensional inference problems:

    library(FIHR)

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
    X <- MASS::mvrnorm(n,mu,Cov)
    y = X%*%beta + rnorm(n)
    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est = FIHR::LF(X = X, y = y, loading = loading, intercept = TRUE)
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    Est$prop.est
    #>          [,1]
    #> [1,] 4.380316
    Est$se
    #> [1] 1.961777
    Est$CI
    #> [1] 0.5353037 8.2253276
    Est$decision
    #> [1] 1

Individualised Treatment Effect in high-dimensional logistic regression
model


    library(MASS)
    n1 = 100
    p = 400
    n2 = 150
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
    beta1 <- rep(0,p)
    beta1[1:10] <- c(1:10)/5
    beta2 <- rep(0,p)
    beta2[1:5] <- c(1:5)/10
    X1 <- MASS::mvrnorm(n1,mu,Cov)
    X2 <- MASS::mvrnorm(n2,mu,Cov)
    y1 = X1%*%beta1 + rnorm(n1)
    y2 = X2%*%beta2 + rnorm(n2)
    loading <- MASS::mvrnorm(1,rep(0,p),Cov)
    Est <- FIHR::ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1259583
    Est$prop.est
    #>            [,1]
    #> [1,] -0.5486188
    Est$se
    #> [1] 2.430959
    Est$CI
    #> [1] -5.313210  4.215973
    Est$decision
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
    Est = FIHR::LF_logistic(X = X, y = y, loading = loading, intercept = TRUE, weight = rep(1,n))
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    Est$prop.est
    #> [1] 0.01001974
    Est$se
    #>           [,1]
    #> [1,] 0.7523157
    Est$CI
    #> [1] 0.0005299749 0.1619073744
    Est$decision
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
    n2 = 200
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
    Est <- FIHR::ITE_Logistic(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    #> [1] 4
    #> [1] 4
    #> [1] 3
    #> [1] "step is 4"
    #> [1] "fixed mu"
    #> [1] 0.07272207
    Est$prop.est
    #> [1] -0.4354605
    Est$se
    #>           [,1]
    #> [1,] 0.8355575
    Est$CI
    #> [1] -2.073123  1.202202
    Est$decision
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
    X <- MASS::mvrnorm(100,mu,Cov)
    y <- X%*%beta + rnorm(100)
    test.set <- c(30:100)

    ## Inference for Quadratic Functional with Population Covariance Matrix in middle

    Est = FIHR::QF(X = X, y = y, G=test.set)
    #> [1] 5
    #> [1] 5
    #> [1] 5
    #> [1] "fixed mu"
    #> [1] 0.1421723
    #> [1] "step is 5"
    Est$prop.est
    #>           [,1]
    #> [1,] 0.2806967
    Est$se
    #> [1] 0.1135306
    Est$CI
    #>            [,1]      [,2]
    #> [1,] 0.05818081 0.5032125
    Est$decision
    #> [1] 1

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = FIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:400,400))
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] 4
    #> [1] 5
    #> [1] 4
    #> [1] "fixed mu"
    #> [1] 0.1777154
    #> [1] "step is 4"
    Est$prop.est
    #>          [,1]
    #> [1,] 10.69138
    Est$se
    #> [1] 1.742625
    Est$CI
    #>        [,1]     [,2]
    #> [1,] 7.2759 14.10687
    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = FIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE)
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] 4
    #> [1] 4
    #> [1] 4
    #> [1] "fixed mu"
    #> [1] 0.1777154
    #> [1] "step is 4"
    Est$prop.est
    #>           [,1]
    #> [1,] 0.2900784
    Est$se
    #> [1] 0.1111879
    Est$CI
    #>            [,1]      [,2]
    #> [1,] 0.07215412 0.5080027
    Est$decision
    #> [1] 1

Finding projection direction in high dimensional linear regression


    n = 100
    p = 400
    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
    resol = 1.5
    step = 3

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- FIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)})
    #> [1] "fixed mu"
    #> [1] 0.1542347

    ## Finding Projection Direction using best step size

    Direction.est <- FIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))
    #> [1] 3
    Direction.est$proj
    #>   [1]  6.367043e-01  3.175779e-02 -5.462947e-22 -5.284029e-22 -1.444957e-21
    #>   [6] -9.855896e-22 -1.051552e-21  8.083174e-22 -1.078752e-02  3.913468e-02
    #>  [11] -4.503133e-02  1.081823e-21  4.528308e-22  6.819856e-02 -2.084241e-22
    #>  [16]  6.708609e-22 -1.750270e-21 -8.181153e-22 -8.739457e-22 -1.602810e-22
    #>  [21]  5.960023e-22  3.495937e-22 -8.039948e-22 -7.504116e-22 -5.919429e-23
    #>  [26]  3.605156e-22  2.714354e-22  7.368755e-22 -4.342021e-22  1.102296e-21
    #>  [31]  1.966371e-22 -5.207015e-22  1.299517e-21 -1.103766e-21  6.393718e-22
    #>  [36] -1.046812e-22 -8.720838e-22  9.507766e-22  2.703072e-23 -7.968778e-22
    #>  [41]  1.259521e-21 -1.324377e-21 -9.219405e-22 -4.648151e-02 -2.568624e-22
    #>  [46] -5.397424e-22  3.494656e-22  1.053274e-22 -2.679896e-22 -2.545201e-22
    #>  [51] -7.484599e-22  7.216520e-22 -1.195804e-22  4.429677e-22 -1.014516e-21
    #>  [56]  1.900919e-22  9.797489e-23 -4.514403e-22  6.843751e-23 -1.820751e-22
    #>  [61] -3.407880e-22 -4.202715e-22 -1.177976e-22 -3.432223e-04 -1.568404e-21
    #>  [66]  1.484010e-21  1.579100e-23 -6.763442e-22  5.823466e-22 -3.773300e-02
    #>  [71]  1.035570e-21 -2.012533e-22  2.075815e-21 -9.069968e-22  1.549440e-22
    #>  [76] -7.170341e-22 -5.219597e-22 -6.801546e-22  8.376773e-22  6.884176e-22
    #>  [81]  5.389911e-22  4.106517e-22 -5.205291e-22  5.346795e-22 -8.123517e-23
    #>  [86] -9.993370e-22 -1.357364e-21  9.391926e-23  5.200438e-22  1.404526e-22
    #>  [91]  1.047197e-21 -4.570880e-02 -5.616079e-22  5.159154e-22 -4.611149e-22
    #>  [96]  1.308558e-21 -4.468825e-22  4.743257e-22 -5.094822e-22 -1.355685e-21
    #> [101]  8.958333e-22 -6.974480e-22  6.555345e-22 -8.938297e-22  1.479035e-21
    #> [106]  1.524848e-22 -1.334904e-21 -4.910989e-22 -2.110547e-22 -1.172052e-21
    #> [111] -7.997769e-22  1.491989e-21  3.929415e-22 -1.114763e-21 -2.491868e-22
    #> [116] -8.935556e-22  1.101184e-21 -5.930786e-22 -1.138064e-21  6.715721e-22
    #> [121] -7.730974e-22  8.985146e-22  5.357683e-22 -3.374570e-22  4.294938e-22
    #> [126] -3.746074e-22 -3.348921e-22 -5.335255e-22 -6.659237e-22  1.731501e-21
    #> [131] -1.012040e-21 -9.808210e-22 -3.633576e-03 -5.079749e-22 -9.867510e-22
    #> [136]  1.680497e-21 -1.498579e-21 -2.919587e-22  2.844552e-03  1.204910e-21
    #> [141]  7.861774e-22 -9.998362e-22  2.390348e-22 -2.935115e-22  2.493400e-22
    #> [146] -1.480679e-21  5.067593e-22 -6.686354e-22 -1.294183e-21 -1.159198e-22
    #> [151] -6.137259e-22  4.964049e-22  1.412984e-21  5.586603e-22  1.106183e-22
    #> [156]  1.139373e-21  5.890066e-22  2.594538e-23  3.806585e-22  3.789383e-22
    #> [161]  4.020720e-22  1.251084e-21 -1.429487e-22 -3.686967e-22  2.680067e-02
    #> [166]  8.103440e-22  1.962727e-21  2.468044e-22 -6.829238e-22 -3.062016e-22
    #> [171]  9.573865e-22  1.102988e-21 -1.603850e-22 -3.577186e-02  3.292722e-22
    #> [176] -2.027053e-22 -3.703741e-22  4.237901e-02 -1.963459e-22  7.468793e-22
    #> [181]  2.017439e-22  7.363366e-22 -9.754414e-22  1.644270e-02  4.319702e-22
    #> [186] -5.390911e-23 -8.403467e-22 -2.704618e-23  4.696035e-22  1.000928e-21
    #> [191]  1.494025e-22 -3.482254e-23  1.703483e-21 -1.455847e-23 -5.418398e-22
    #> [196]  5.053500e-22  1.901827e-22 -1.103933e-21  3.119558e-22 -2.780393e-22
    #> [201]  1.580803e-03 -6.934525e-22 -2.863739e-22 -1.761245e-21  1.737149e-21
    #> [206] -1.957264e-21  1.010290e-21 -1.526568e-21  6.089806e-23  6.680559e-22
    #> [211] -9.232928e-03 -1.240450e-21 -2.601409e-22  1.747407e-22 -2.974047e-22
    #> [216] -1.059547e-22 -3.752011e-22 -5.221251e-22  4.637780e-22 -4.072295e-23
    #> [221] -9.184730e-22 -7.337057e-22 -1.727801e-03  5.013619e-23 -6.117447e-22
    #> [226] -8.536727e-22  8.643009e-22 -8.009666e-22  1.123676e-21 -8.892662e-22
    #> [231] -4.771038e-22 -4.271280e-22 -1.802819e-21  4.849793e-22 -4.193867e-22
    #> [236] -3.300124e-22 -1.258518e-22  6.413504e-22  1.779715e-21  5.658024e-22
    #> [241] -4.878924e-02 -5.696402e-22  3.622163e-22  1.334874e-21 -1.245878e-21
    #> [246]  5.740235e-22  1.142683e-22  1.239938e-22 -9.293025e-22 -2.173530e-22
    #> [251] -7.237769e-22  1.205556e-21 -3.980802e-22  1.445931e-22 -1.094873e-21
    #> [256]  6.409253e-22  5.343114e-23  1.061455e-21  1.277724e-21 -3.163014e-22
    #> [261]  3.603045e-23  1.192724e-21 -3.930845e-22 -2.002166e-22 -3.713694e-22
    #> [266]  2.355287e-03 -1.949077e-23  1.269568e-22  3.435924e-22 -5.420507e-22
    #> [271] -1.109403e-21 -1.710754e-21 -1.601256e-22 -3.975498e-22 -6.105823e-23
    #> [276] -1.423646e-21 -3.422190e-22  4.902283e-22 -1.145291e-21  1.280229e-22
    #> [281] -7.019932e-23  6.464173e-22  1.002250e-21 -4.499641e-23  6.141331e-22
    #> [286] -1.019640e-21 -9.263131e-22  7.004603e-22  4.675601e-22  5.563660e-22
    #> [291]  1.352915e-22  6.734570e-22 -6.427957e-23  5.449054e-23 -5.438263e-22
    #> [296] -1.166513e-21  5.858795e-23 -1.571505e-22 -8.844374e-23 -2.146897e-21
    #> [301]  1.197316e-22  4.017124e-22  8.632193e-22 -8.737131e-22 -7.698160e-22
    #> [306] -4.106514e-22  8.634978e-04  2.306522e-22 -5.763747e-22  2.054271e-02
    #> [311] -4.115801e-22  6.782607e-02  1.270346e-21  5.401182e-25  1.869127e-22
    #> [316]  6.443835e-22  8.148221e-22  2.955899e-22 -4.195940e-22 -5.774712e-22
    #> [321] -8.756456e-03 -4.109861e-22  1.815414e-23  7.425035e-22 -1.159358e-21
    #> [326] -1.929661e-22  3.484096e-22 -3.915287e-22 -1.857374e-02  4.914947e-22
    #> [331] -5.969602e-25 -1.060170e-21  9.747766e-22 -8.984721e-22 -2.128481e-22
    #> [336] -1.129373e-21  9.282037e-22  2.439721e-22  5.929511e-22  1.128496e-21
    #> [341]  1.114117e-21  3.457630e-22 -1.786434e-22 -6.732628e-23 -1.005085e-21
    #> [346]  1.060345e-02 -8.658427e-23 -4.858286e-22 -4.017353e-02  5.518550e-22
    #> [351] -6.048490e-23 -1.478767e-21 -5.709502e-22  1.784295e-02  3.907553e-02
    #> [356] -1.048302e-21 -1.234986e-22  3.696846e-22  4.350473e-22 -3.881050e-02
    #> [361] -1.079845e-22 -3.319523e-22  5.231790e-23  6.099922e-22 -2.105517e-22
    #> [366] -4.529919e-22  1.148124e-21  9.314583e-23 -1.574029e-22  8.718379e-22
    #> [371] -1.663761e-22 -4.453050e-22  4.651530e-22  6.394913e-22 -5.981821e-22
    #> [376] -1.787914e-22 -7.215119e-22 -3.468273e-22  1.800184e-23  9.547768e-22
    #> [381] -4.659070e-22 -1.694090e-21 -1.409345e-24 -1.159887e-21  9.020592e-22
    #> [386]  5.165774e-22  4.332198e-02 -4.855378e-22  1.008411e-21  1.185932e-22
    #> [391]  1.000698e-21 -8.475052e-22 -1.251901e-02  4.784787e-22  3.341784e-22
    #> [396] -3.800361e-22  7.624471e-22 -8.638127e-22  6.148154e-22 -1.186068e-22

Finding projection direction in high dimensional logistic regression


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

    Direction.est <- FIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)},model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)
    #> [1] "fixed mu"
    #> [1] 0.2181208

    ## Finding Projection Direction using best step size

    Direction.est <- FIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)
    #> [1] 2


<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIHR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIHR is to provide inference procedures in the
high-dimensional setting for (1) linear functionals (LF) and quadratic
functionals (QF) in linear regression, (2) linear functional in logistic
regression (LF_logistic), (3) individual treatment effects (ITE) in linear and
logistic regression.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("prabrishar1/SIHR")

## Example

These are basic examples which show how to use the package to conduct 
high-dimensional inference:

    library(SIHR)

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
    Est = SIHR::LF(X = X, y = y, loading = loading, intercept = TRUE)
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    Est$prop.est
    #>          [,1]
    #> [1,] 8.754123
    Est$se
    #> [1] 1.961673
    Est$CI
    #> [1]  4.909315 12.598932
    Est$decision
    #> [1] 1

Individualized Treatment Effect in high-dimensional logistic regression
model


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
    #> [1] 0.1542668
    Est$prop.est
    #>           [,1]
    #> [1,] -6.921414
    Est$se
    #> [1] 2.356758
    Est$CI
    #> [1] -11.540575  -2.302254
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
    Est = SIHR::LF_logistic(X = X, y = y, loading = loading, intercept = TRUE, weight = rep(1,n))
    #> [1] 3
    #> [1] 3
    #> [1] 3
    #> [1] "step is 3"
    #> [1] "fixed mu"
    #> [1] 0.1542668
    Est$prop.est
    #> [1] 0.8656799
    Est$se
    #>           [,1]
    #> [1,] 0.5817977
    Est$CI
    #> [1] 0.3971592 0.9843867
    Est$decision
    #> [1] 1

Individualized Treatment Effect in high-dimensional logistic model

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
    #> [1] 0.1542668
    Est$prop.est
    #> [1] -0.8057362
    Est$se
    #>           [,1]
    #> [1,] 0.9111076
    Est$CI
    #> [1] -2.5914742  0.9800018
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

    Est = SIHR::QF(X = X, y = y, G=test.set)
    #> [1] 5
    #> [1] 5
    #> [1] 5
    #> [1] "fixed mu"
    #> [1] 0.1421723
    #> [1] "step is 5"
    #> Warning in SIHR::QF(X = X, y = y, G = test.set): The model is most likely
    #> misspecified because the correction term is larger than the lasso estimate in
    #> absolute value. See cluster or group: X30, X31, X32, X33, X34, X35, X36, X37,
    #> X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53,
    #> X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69,
    #> X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85,
    #> X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100 . The
    #> value of the lasso.plugin and correction are 0.02344 respectively 0.07335 .
    Est$prop.est
    #>            [,1]
    #> [1,] 0.09678704
    Est$se
    #> [1] 0.1028702
    Est$CI
    #>            [,1]     [,2]
    #> [1,] -0.1048349 0.298409
    Est$decision
    #> [1] 0

    ## Inference for Quadratic Functional with known matrix A in middle

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE,A = diag(1:400,400))
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] 4
    #> [1] 5
    #> [1] 4
    #> [1] "fixed mu"
    #> [1] 0.1777154
    #> [1] "step is 4"
    Est$prop.est
    #>          [,1]
    #> [1,] 2.610302
    Est$se
    #> [1] 0.8137623
    Est$CI
    #>          [,1]     [,2]
    #> [1,] 1.015358 4.205247
    Est$decision
    #> [1] 1

    ## Inference for square norm of regression vector

    Est = SIHR::QF(X = X, y = y, G=test.set, Cov.weight = FALSE)
    #> [1] "Warning : Matrix A in the quadratic form is taken as the identity matrix"
    #> [1] 5
    #> [1] 4
    #> [1] 4
    #> [1] "fixed mu"
    #> [1] 0.1777154
    #> [1] "step is 4"
    Est$prop.est
    #>            [,1]
    #> [1,] 0.08681543
    Est$se
    #> [1] 0.1024298
    Est$CI
    #>            [,1]      [,2]
    #> [1,] -0.1139432 0.2875741
    Est$decision
    #> [1] 0

Constructing projection directions in high dimensional linear regression


    n = 100
    p = 400
    X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
    resol = 1.5
    step = 3

    ## Finding Projection Direction using fixed tuning parameter

    Direction.est <- SIHR::Direction_fixedtuning(X,loading=c(1,rep(0,(p-1))),mu=sqrt(2.01*log(p)/n)*resol^{-(step-1)})
    #> [1] "fixed mu"
    #> [1] 0.1542347

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))
    #> [1] 3
    Direction.est$proj
    #>   [1]  6.153863e-01  1.420175e-22  1.812455e-02 -1.867472e-22 -1.151811e-21
    #>   [6] -1.889849e-21  7.643401e-03  1.228272e-21 -7.546314e-22  1.674520e-21
    #>  [11] -4.092914e-22 -2.589979e-02  9.246626e-22  2.295892e-21  9.593602e-22
    #>  [16] -7.359118e-22 -2.851292e-03 -1.862691e-02  6.990818e-22  1.895041e-03
    #>  [21]  4.075018e-22 -4.184164e-22  1.102489e-21 -1.723396e-21 -7.293593e-22
    #>  [26] -2.485113e-21 -9.442043e-22  1.418892e-02 -7.220306e-22  1.106877e-21
    #>  [31]  1.717093e-21 -1.177397e-21  2.628007e-21 -2.366763e-21  6.857750e-22
    #>  [36]  1.275047e-21 -4.479678e-22  8.190668e-22 -1.430923e-21  4.143562e-22
    #>  [41] -2.908977e-21 -1.460223e-22  1.857951e-21  8.957426e-22  1.288449e-21
    #>  [46] -9.082935e-22 -5.777117e-22 -1.669370e-21 -1.549366e-21  6.010710e-22
    #>  [51]  1.272371e-22 -5.600599e-22  7.043514e-22  1.852485e-02  1.721818e-21
    #>  [56] -3.638629e-22  2.502178e-21  4.517224e-22  3.827135e-02  5.169460e-22
    #>  [61] -2.126055e-21  1.699745e-21  7.678307e-22 -1.605222e-21  2.535009e-22
    #>  [66] -1.431515e-21 -1.116932e-21  1.110926e-05 -1.459922e-21  1.359307e-21
    #>  [71] -1.982331e-02 -1.621720e-21  6.120440e-22 -1.429610e-21 -3.844135e-22
    #>  [76] -8.168642e-23  3.110311e-22 -2.884457e-22  2.405911e-21 -1.048347e-02
    #>  [81] -5.340699e-02  1.567628e-22  1.831085e-22  1.505941e-21  1.783649e-02
    #>  [86]  2.179671e-22  2.888240e-22  4.906938e-22  7.808067e-02  7.953271e-23
    #>  [91]  8.979873e-22  1.123540e-21 -7.301066e-02 -1.544843e-21 -2.125353e-22
    #>  [96] -2.751136e-22  7.844654e-22 -1.135009e-21  5.559081e-23  1.085240e-21
    #> [101]  1.171077e-22  2.140230e-22  1.498549e-21 -1.525131e-21 -2.307585e-22
    #> [106] -1.067209e-22  9.146301e-22  8.036471e-22  1.567159e-21 -8.030335e-22
    #> [111] -1.527339e-21  4.018928e-22 -3.967709e-22 -4.835329e-22 -2.029934e-02
    #> [116]  1.086620e-21  1.048839e-21 -2.121374e-03  2.426092e-21  4.430049e-22
    #> [121] -4.042872e-22 -1.624651e-21  6.258935e-22 -4.926947e-22  5.876260e-22
    #> [126]  8.807756e-22 -2.086620e-21  7.548818e-22  2.861558e-22  1.370502e-21
    #> [131]  1.202665e-21 -1.885008e-21 -3.639943e-21  5.093995e-22  1.187337e-21
    #> [136] -9.159397e-22  1.987759e-21 -1.525037e-21 -1.918040e-21 -6.451236e-22
    #> [141] -1.405290e-21  1.293256e-21  1.058590e-21  1.256132e-21 -1.965083e-21
    #> [146]  1.942020e-21 -3.105835e-22  2.575627e-02 -4.149437e-22  4.020713e-22
    #> [151]  2.211744e-03  1.360607e-21 -1.936026e-21 -2.108958e-21 -1.964524e-21
    #> [156] -5.236840e-22 -1.650069e-21 -9.102558e-22  1.178555e-22 -1.465825e-21
    #> [161] -2.537326e-21 -3.539186e-22 -9.084276e-22  1.679537e-21  1.122039e-21
    #> [166] -9.481736e-22 -8.889386e-22  7.899339e-22 -1.962320e-21  5.001305e-22
    #> [171] -2.005510e-23  5.720211e-22 -8.430097e-22 -7.940741e-22 -4.720765e-22
    #> [176]  3.883682e-22  6.396280e-22  4.968226e-22  7.652114e-22 -1.393527e-21
    #> [181]  2.030256e-22  3.078642e-21  3.606258e-02  1.615682e-22 -1.484908e-21
    #> [186]  2.741059e-21  8.564333e-22 -1.130609e-21  3.158542e-23  8.669693e-22
    #> [191] -7.310951e-22  8.399206e-22  4.406541e-22  1.110776e-21 -8.666016e-22
    #> [196]  2.896655e-21 -1.736689e-22 -1.310188e-22 -1.637246e-21  3.400723e-22
    #> [201]  1.770712e-22  1.272346e-21  8.984598e-22 -5.066872e-22  3.497617e-21
    #> [206]  1.231189e-21 -1.629833e-21  9.739437e-22  9.513532e-22 -3.138927e-23
    #> [211]  1.768770e-22  1.265735e-21 -4.547119e-03  4.245182e-22 -5.282849e-03
    #> [216] -5.054031e-22 -2.663859e-22 -1.996185e-21  1.071240e-21  2.977299e-02
    #> [221] -1.268395e-21 -6.521012e-22  5.538102e-03 -1.400123e-02  1.617057e-21
    #> [226]  2.141486e-22 -1.714278e-21 -2.140622e-21 -1.674959e-21 -1.596596e-21
    #> [231] -1.049159e-21  1.558777e-21  1.812031e-21  8.849795e-23 -2.068672e-22
    #> [236]  1.296121e-21 -1.158615e-21  6.155592e-22 -1.659527e-21 -1.160278e-21
    #> [241] -2.033767e-22  6.578554e-22  1.819679e-21 -6.020866e-22 -1.634769e-21
    #> [246] -7.150421e-22  1.582658e-22 -2.127728e-22  1.352041e-21  3.437914e-02
    #> [251] -9.120502e-22 -1.575024e-21 -1.558246e-21  1.379496e-21  1.065738e-21
    #> [256] -7.389512e-22  2.339543e-23 -3.230080e-21  3.150217e-22 -1.263387e-21
    #> [261]  1.992417e-21  1.156849e-21  4.492078e-03  2.657560e-22  1.327219e-21
    #> [266]  1.336872e-22 -4.161562e-03  9.233349e-03 -1.113092e-21 -1.115879e-22
    #> [271] -2.326205e-22  2.159424e-21 -5.591220e-22  3.086443e-21 -2.169197e-22
    #> [276] -2.041898e-21 -1.234479e-22  8.164965e-22  1.056295e-21 -3.019909e-22
    #> [281]  3.365900e-21  6.565994e-22 -6.360673e-22  6.652114e-22 -3.543823e-21
    #> [286] -6.260079e-22 -3.165193e-23 -1.489062e-21  1.841092e-02 -6.068682e-02
    #> [291] -5.522904e-22 -2.095851e-21  2.339723e-23 -2.056404e-21 -6.542013e-22
    #> [296] -1.101623e-02 -1.989310e-21  1.747782e-21 -2.395100e-21  6.638690e-22
    #> [301]  1.036840e-21 -1.842092e-21  3.492307e-21  6.608443e-03 -8.573017e-22
    #> [306] -1.470385e-21  2.251066e-21 -3.010584e-22  5.459693e-22 -9.324596e-22
    #> [311] -1.357321e-21  1.117839e-02 -8.539391e-22 -9.811961e-22  6.302845e-22
    #> [316]  1.209060e-21 -1.073261e-21  5.341449e-02 -1.079478e-02 -1.758486e-21
    #> [321]  2.703203e-22 -1.464303e-21  9.741456e-22  5.359795e-22 -3.228481e-22
    #> [326] -1.227669e-22  7.715332e-22 -3.245738e-02 -1.097583e-21 -5.201793e-23
    #> [331]  1.248811e-21 -8.889716e-22  5.516817e-22  6.331999e-22 -2.709587e-22
    #> [336]  1.413540e-21  1.849655e-22  1.446142e-21  4.449758e-22  1.409309e-21
    #> [341]  2.197691e-21 -2.798552e-22 -8.080141e-22  1.427442e-22  1.396337e-21
    #> [346] -4.595372e-23 -1.200218e-21  3.608354e-22 -1.421190e-21 -1.701096e-21
    #> [351]  1.206605e-21 -2.102615e-02 -4.128993e-21  2.062226e-21  1.032577e-21
    #> [356]  2.626411e-22 -1.942685e-22 -1.784422e-21 -1.817778e-02  9.458530e-22
    #> [361] -4.867141e-02 -9.188646e-22 -1.807932e-21  1.759228e-21  1.386390e-21
    #> [366] -2.832220e-22  3.246121e-22  7.745827e-22 -1.604399e-21 -1.642985e-22
    #> [371] -1.154091e-22  2.985844e-22 -6.407683e-22 -9.949305e-23  1.901995e-21
    #> [376]  1.030075e-21  4.272566e-22  2.224329e-22 -3.132703e-21 -1.124954e-21
    #> [381] -2.156735e-21 -3.824580e-21 -2.031466e-21 -3.173441e-22  1.486855e-21
    #> [386]  7.702068e-22 -1.672242e-21  1.652798e-22 -2.247011e-21  2.848394e-21
    #> [391] -2.176901e-02  6.798921e-22  2.126275e-02  1.308301e-21 -1.438302e-21
    #> [396]  2.276733e-21 -7.116615e-22 -7.854369e-22  1.203616e-22  8.649040e-03

Constructing projection directions in high dimensional logistic regression


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
    #> [1] "fixed mu"
    #> [1] 0.2181208

    ## Finding Projection Direction using best step size

    Direction.est <- SIHR::Direction_searchtuning(Xc,loading,model = "logistic",weight = 1/f_prime, deriv.vec = f_prime)
    #> [1] 2

#' Inference for the case probability or a linear combination of regression coefficients in high dimensional logistic regression.
#'
#' @description
#' Computes the bias corrected estimator of the case probability or the linear combination of regression coefficients in the high dimensional logistic regression model and the corresponding standard error.
#' It also constructs the confidence interval for the case probability or the linear combination and tests whether the case probability is above \eqn{0.5} or not.
#' Here case probability refers to the conditional probability of the binary response variable taking value 1 given the predictors take value \code{loading}.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading Loading, of length \eqn{p}
#' @param weight The weight vector used for bias correction, of length \eqn{n}; if set to \code{NULL}, the weight is the inverse of the first derivative of the logit function (default = \code{NULL})
#' @param trans Should results for the case probability (\code{TRUE}) or the linear combination (\code{FALSE}) be reported (default = \code{TRUE})
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param intercept.loading Should intercept be included for the \code{loading} (default = \code{TRUE})
#' @param init.Lasso Initial LASSO estimator of the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of LASSO estimator of the regression vector (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step The step size used to compute \code{mu}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis that the case probability is less than or equal to 0.5 (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias corrected estimator of the case probability or the linear combination of regression coefficients}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the case probability or the linear combination}
#' \item{decision}{\code{decision}\eqn{=1} implies the case probability is above 0.5 \eqn{\newline}
#' \code{decision}\eqn{=0} implies the case probability is not above 0.5}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator of the case probability or the linear combination}
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlog}{SIHR}
#' @examples
#' \donttest{
#' A1gen <- function(rho,p){
#'  A1 <- matrix(0,p,p)
#'  for(i in 1:p){
#'    for(j in 1:p){
#'      A1[i,j] <- rho^(abs(i-j))
#'    }
#'  }
#'  A1
#' }
#' n <- 100
#' p <- 400
#' mu <- rep(0,p)
#' rho <- 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- 0.5*c(1:10)/10
#' a0 <- 0
#' loading <- c(1,rep(0,(p-1)))
#' X <- MASS::mvrnorm(n,mu,Cov)
#' exp_val <- X%*%beta+a0
#' prob <- exp(exp_val)/(1+exp(exp_val))
#' y <- rbinom(n,1,prob)
#' Est <- LF_logistic(X = X, y = y, loading = loading, trans = TRUE)
#' }
LF_logistic <- function(X, y, loading, weight = NULL, trans = TRUE, intercept = TRUE, intercept.loading = TRUE, init.Lasso = NULL, lambda = NULL, mu = NULL, step = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE){
  xnew <- loading
  X <- as.matrix(X)
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    stop("Error: Check dimensions of X and y")
  } else {
    data <- na.omit(data.frame(y,X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X)
    n <- nrow(X)
    mean = colMeans(X)
    M = matrix(rep(mean,nrow(X)),byrow = T, nrow = nrow(X), ncol = ncol(X))
    X = X - M
    if(intercept.loading == TRUE){
      xnew = xnew - mean
    }
    col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X)+0.0001)
    Xnor <- X %*% diag(col.norm)
    if(is.null(init.Lasso))
    {
      init.Lasso <-  Initialization.step_log(X,y,lambda,intercept)
      htheta <- init.Lasso$lasso.est
    } else {
      htheta <- init.Lasso
    }

    if (intercept==TRUE){
      Xb <- cbind(rep(1,n),Xnor)
      Xc <- cbind(rep(1,n),X)
      col.norm <- c(1,col.norm)
      pp <- (p+1)
    } else {
      Xb <- Xnor
      Xc <- X
      pp <- p
    }

    sparsity <- sum(abs(htheta) > 0.001)
    sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / max(0.9*n, n - sparsity))

    if(intercept == TRUE){
      loading <- rep(0,pp)
      if(intercept.loading == TRUE){
        loading[1] <- 1
      }
      if(intercept.loading == FALSE){
        loading[1] <- 0
      }
      loading[-1] <- xnew
    } else {
      if(intercept.loading == TRUE){
        print(paste("Setting intercept = FALSE and intercept.loading = FALSE"))
      }
      loading <- xnew
    }
    loading.norm <- sqrt(sum(loading^2))
    lasso.plugin <- sum(loading*htheta)
    deriv.vec <- exp(Xc%*%htheta)/(1+exp(Xc%*%htheta))^2

    if(is.null(weight)){
      weight <- 1/deriv.vec
    }
    X.weight = diag(c(sqrt(deriv.vec*weight))) %*% Xc ####modified

    count=0
    for(i in 1:ncol(X)){
      if(length(unique(X[,i])) == 1){
        count = count+1
      }
    }
    if(count!=0 && intercept==TRUE)
    {
      stop("Data is singular")
    }else{
      if ((n >= 6*p)){
        gamma.hat <- (1/n)*(t(X.weight)%*%X.weight)
        tmp <- eigen(gamma.hat)
        tmp <- min(tmp$values)/max(tmp$values)
      }else{
        tmp <- 0
      }

      if ((n >= 6*p) && (tmp >= 1e-4)){
        direction <- solve(gamma.hat)%*%loading/loading.norm
      }else{
        if(is.null(step)){
          step.vec<-rep(NA,3)
          for(t in 1:3){
            index.sel <- sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
            Direction.Est.temp <-  Direction_searchtuning_glm(Xc[index.sel,], loading, mu = NULL, weight = weight[index.sel], deriv.vec = deriv.vec[index.sel], resol, maxiter)
            step.vec[t] <- Direction.Est.temp$step
          }
          step<- getmode_log(step.vec)
        }
        Direction.Est <-  Direction_fixedtuning_glm(Xc, loading, mu = sqrt(2.01*log(pp)/n)*resol^{-(step-1)}, weight = weight, deriv.vec = deriv.vec)

        while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
          step <- step-1
          Direction.Est <-  Direction_fixedtuning_glm(Xc, loading, mu = sqrt(2.01*log(pp)/n)*resol^{-(step-1)}, weight = weight, deriv.vec = deriv.vec)
        }
        if(verbose == TRUE){
          print(paste("step is", step))
        }
        direction <- Direction.Est$proj
      }
      exp_pred <- Xc%*%(htheta)

      weighed.residual <- (y - exp(exp_pred)/(1+ exp(exp_pred)))*weight

      correction <- sum((Xc%*%direction)*weighed.residual)/n
      debias.est <- lasso.plugin+correction*loading.norm
      rho_hat <- exp(debias.est)/(1+exp(debias.est))^2
      se_linear <- sqrt(mean((Xc%*%direction)^2*weight^2*deriv.vec))*loading.norm/sqrt(n)
      CI_linear <- c(debias.est - qnorm(1-alpha/2)*se_linear, debias.est + qnorm(1-alpha/2)*se_linear)
      if(debias.est - qnorm(1-alpha)*se_linear > 0){
        dec <- 1
      }else{
        dec <- 0
      }
      if(trans == TRUE){
        prop.est = expo(debias.est)
        se = rho_hat*se_linear
        CI = c(expo(CI_linear[1]),expo(CI_linear[2]))
        plug.in = expo(lasso.plugin)
      }else{
        prop.est = debias.est
        se = se_linear
        CI = CI_linear
        plug.in = lasso.plugin
      }
      returnList <- list("prop.est" = prop.est,
                         "se" = se,
                         "CI" = CI,
                         "decision" = dec,
                         "proj"=direction,
                         "plug.in"= plug.in
      )
      return(returnList)
    }
  }
}

#' Inference for difference of case probabilities in high dimensional logistic regressions
#'
#' @description
#' Computes the bias corrected estimator of the difference between case probabilities or a linear combination of the difference between two regression vectors with respect to two high dimensional logistic regression models
#' and the corresponding standard error. It also constructs the confidence interval for the difference of case probabilities or a linear combination of the difference between the regression vectors and test
#' whether it is above zero or not. Here the case probability refers to the conditional probability of the binary response variable taking value 1 given the predictors are assigned to \code{loading}.
#'
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_2}
#' @param loading Loading, of length \eqn{p}
#' @param weight The weight vector used for bias correction, of length \eqn{n}; if set to \code{NULL}, the weight is
#' the inverse of the first derivative of the logit function (default = \code{NULL})
#' @param trans Should results for the case probability (\code{TRUE}) or the linear combination (\code{FALSE}) be reported (default = \code{TRUE})
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param intercept.loading Should intercept be included for the \code{loading} (default = \code{TRUE})
#' @param init.Lasso1 Initial LASSO estimator of the first regression vector (default = \code{NULL})
#' @param init.Lasso2 Initial LASSO estimator of the second regression vector (default = \code{NULL})
#' @param lambda1 The tuning parameter in the construction of LASSO estimator of the first regression vector (default = \code{NULL})
#' @param lambda2 The tuning parameter in the construction of LASSO estimator of the second regression vector (default = \code{NULL})
#' @param mu1 The dual tuning parameter used in the construction of the first projection direction (default = \code{NULL})
#' @param mu2 The dual tuning parameter used in the construction of the second projection direction (default = \code{NULL})
#' @param step1 The step size used to compute \code{mu1}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu1}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param step2 The step size used to compute \code{mu2}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu2}
#' such that the dual optimization problem for constructing the second projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu1} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 6)
#' @param alpha Level ofsignificance to test the null hypothesis which claims that the first case probability is not greater than the second case probability (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator for the difference between case probabilities or the linear combination of the difference between two regression vectors}
#' \item{se}{The standard error for the bias-corrected estimator}
#' \item{CI}{The confidence interval for the difference between case probabilities or the linear combination of the difference between two regression vectors}
#' \item{decision}{\code{decision}\eqn{=1} implies the first case probability or linear combination is greater than the second one\eqn{\newline}
#' \code{decision}\eqn{=0} implies the first case probability or linear combination is less than the second one}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit
#' @import CVXR Matrix glmnet
#' @examples
#' \donttest{
#' A1gen <- function(rho,p){
#' A1 <- matrix(0,p,p)
#' for(i in 1:p){
#'   for(j in 1:p){
#'     A1[i,j] <- rho^(abs(i-j))
#'   }
#' }
#' A1
#' }
#' n1 <- 100
#' n2 <- 100
#' p <- 400
#' mu <- rep(0,p)
#' rho <- 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta1 <- rep(0,p)
#' beta1[1:10] <- c(1:10)/5
#' beta2 <- rep(0,p)
#' beta2[1:5] <- c(1:5)/10
#' X1 <- MASS::mvrnorm(n1,mu,Cov)
#' X2 <- MASS::mvrnorm(n2,mu,Cov)
#' exp_val1 <- X1%*%beta1
#' exp_val2 <- X2%*%beta2
#' prob1 <- exp(exp_val1)/(1+exp(exp_val1))
#' prob2 <- exp(exp_val2)/(1+exp(exp_val2))
#' y1 <- rbinom(n1,1,prob1)
#' y2 <- rbinom(n2,1,prob2)
#' loading <- c(1,rep(0,(p-1)))
#' Est <- ITE_Logistic(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, trans = FALSE)
#' }
ITE_Logistic <- function(X1, y1, X2, y2, loading, weight = NULL, trans = TRUE, intercept = TRUE, intercept.loading = TRUE, init.Lasso1 = NULL, init.Lasso2 = NULL, lambda1 = NULL, lambda2 = NULL, mu1 = NULL, mu2 = NULL, step1 = NULL, step2 = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE){
  Est1 <- LF_logistic(X=X1, y=y1, loading = loading, weight = weight, trans = FALSE, intercept = intercept, intercept.loading = intercept.loading, init.Lasso = init.Lasso1, lambda = lambda1, mu = mu1, step = step1, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  Est2 <- LF_logistic(X=X2, y=y2, loading = loading, weight = weight, trans = FALSE, intercept = intercept, intercept.loading = intercept.loading, init.Lasso = init.Lasso2, lambda = lambda2, mu = mu2, step = step2, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  if(trans == TRUE){
    prop.est = expo(Est1$prop.est) - expo(Est2$prop.est)
    se <- sqrt(((exp(Est1$prop.est)/(1+exp(Est1$prop.est))^2)*Est1$se)^2 + ((exp(Est2$prop.est)/(1+exp(Est2$prop.est))^2)*Est2$se)^2)
  }
  else{
    prop.est <- Est1$prop.est - Est2$prop.est
    se <- sqrt((Est1$se)^2 + (Est2$se)^2)
  }
  CI <- c((Est1$prop.est - Est2$prop.est) - qnorm(1-alpha/2)*sqrt((Est1$se)^2 + (Est2$se)^2), (Est1$prop.est - Est2$prop.est) + qnorm(1-alpha/2)*sqrt((Est1$se)^2 + (Est2$se)^2))
  if(trans == TRUE){
    if(CI[1]<-1){
      CI[1] = -1
    }
    if(CI[2] > 1){
      CI[2] = 1
    }
  }else{
    CI = CI
  }
  if((Est1$prop.est - Est2$prop.est) - qnorm(1-alpha)*sqrt((Est1$se)^2 + (Est2$se)^2) > 0){
    dec <- 1
  }else{
    dec <- 0
  }
  returnList <- list("prop.est" = prop.est,
                     "se"=se,
                     "CI"=CI,
                     "decision" = dec
  )
  return(returnList)
}

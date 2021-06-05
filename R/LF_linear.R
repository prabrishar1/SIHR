#' Inference for a linear combination of regression coefficients in high dimensional linear regression.
#'
#' @description
#' Computes the bias corrected estimator of the linear combination of regression coefficients and the corresponding standard error.
#' It also constructs the confidence interval for the linear combination of regression coefficients and test whether it is above zero or not.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading Loading, of length \eqn{p}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param center Should the design matrix \code{X} and \code{loading} be centered (default = \code{FALSE})
#' @param init.Lasso Initial LASSO estimator of the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter in the construction of LASSO estimator of the regression vector (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step The step size used to compute \code{mu}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis which claims that the linear combination of the regression coefficients
#' is less than or equal to zero (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator for the linear combination of regression coefficients}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the linear combination of regression coefficients}
#' \item{decision}{\code{decision}\eqn{=1} implies the linear combination of regression coefficients is above zero \eqn{\newline}
#' \code{decision}\eqn{=0} implies the linear combination of regression coefficients is not above zero}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator for the linear combination of regression coefficients}
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm na.omit
#' @importFrom scalreg scalreg
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlin}{SIHR}
#'
#' @examples
#' n <- 90
#' p <- 200
#' A1gen <- function(rho,p){
#' A1=matrix(0,p,p)
#' for(i in 1:p){
#'  for(j in 1:p){
#'    A1[i,j] <- rho^(abs(i-j))
#'  }
#' }
#' A1
#' }
#' mu <- rep(0,p)
#' rho <- 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- c(1:10)/5
#' X <- MASS::mvrnorm(n,mu,Cov)
#' y <- X%*%beta + rnorm(n)
#' loading <- c(1,rep(0,(p-1)))
#' Est <- LF(X = X, y = y, loading = loading, intercept = TRUE)
LF <- function(X, y,loading, intercept = TRUE, center = FALSE, init.Lasso = NULL, lambda = NULL, mu = NULL, step = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE){
  xnew <- loading
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    stop("Error : Check dimensions of X and y")
  } else {
    data <- na.omit(data.frame(y, X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X)
    n <- nrow(X)
    if(center == TRUE){
      mean = colMeans(X)
      M = matrix(rep(mean,nrow(X)),byrow = T, nrow = nrow(X), ncol = ncol(X))
      X = X - M
      xnew = xnew - mean
    }else{
      X = X
      xnew = xnew
    }
    col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
    Xnor <- X %*% diag(col.norm)
    if(is.null(init.Lasso)){
      ####### implement a lasso algorithm to get beta and sigma
      init.Lasso <-  Initialization.step(X, y, lambda, intercept)
      htheta <- init.Lasso$lasso.est
    } else {
      htheta <- init.Lasso
    }
    if (intercept == TRUE){
      Xb <- cbind(rep(1,n),Xnor)
      Xc <- cbind(rep(1,n),X)
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
      loading[1] <- 1
      loading[-1] <- xnew
    } else {
      loading <- xnew
    }
    loading.norm <- sqrt(sum(loading^2))
    lasso.plugin <- sum(loading*htheta)

    count <- 0
    for(i in 1:ncol(X)){
      if(length(unique(X[,i])) == 1){
        count <- count + 1
      }
    }
    if(count!=0 && intercept == TRUE)
    {
      stop("Data is singular")
    } else {
      if ((n >= 6*p)){
        sigma.hat <- (1/n)*(t(Xc)%*%Xc)
        tmp <- eigen(sigma.hat)
        tmp <- min(tmp$values)/max(tmp$values)
      } else {
        tmp <- 0
      }
      sigma.hat <- (1/n)*(t(Xc)%*%Xc)
      if ((n >= 6*p) && (tmp >= 1e-4)){
        direction <- solve(sigma.hat)%*%loading
      } else {
        if(is.null(step)){
          step.vec <- rep(NA,3)
          for(t in 1:3){
            index.sel <- sample(1:n, size=ceiling(0.5*min(n,p)), replace=FALSE)
            Direction.Est.temp <-  Direction_searchtuning_lin(Xc[index.sel,], loading, mu = NULL, resol, maxiter)
            step.vec[t] <- Direction.Est.temp$step
          }
          step <-  getmode(step.vec)
        }
        Direction.Est<- Direction_fixedtuning_lin(Xc, loading, mu = sqrt(2.01*log(pp)/n)*resol^{-(step-1)})
        while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
          step <- step-1
          Direction.Est <-  Direction_fixedtuning_lin(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
        }
        if(verbose == TRUE){
          print(paste("step is", step))
        }
        direction <- Direction.Est$proj
      }
      correction <- t(Xc%*%direction)%*%(y - Xc%*%htheta)/n
      debias.est <- lasso.plugin + correction*loading.norm
      se <- sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
      CI <- c(debias.est - qnorm(1-alpha/2)*se, debias.est + qnorm(1-alpha/2)*se)
      if(debias.est - qnorm(1-alpha)*se > 0){
        dec <- 1
      }else{
        dec <- 0
      }
      returnList <- list("prop.est" = debias.est,
                         "se" = se,
                         "CI" = CI,
                         "decision" = dec,
                         "proj" = direction,
                         "plug.in" = lasso.plugin
      )
      return(returnList)
    }
  }
}

#' Individualized treatment effect in the high dimensional linear regression
#'
#' @description
#' Computes the bias corrected estimator of the Individualized Treatment Effect (ITE)
#' and the corresponding standard error. It also constructs the confidence interval for ITE and test
#' whether ITE is above zero or not. Here ITE is defined as a linear combination of the difference between two regression vectors.
#'
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_2}
#' @param loading Loading, of length \eqn{p}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param center Should the design matrices \code{X1}, \code{X2} and \code{loading} be centered (default = \code{FALSE})
#' @param init.Lasso1 Initial LASSO estimator of the first regression vector (default = \code{NULL})
#' @param init.Lasso2 Initial LASSO estimator of the second regression vector (default = \code{NULL})
#' @param lambda1 The tuning parameter in the construction of LASSO estimator of the first regression vector (default = \code{NULL})
#' @param lambda2 The tuning parameter in the construction of LASSO estimator of the second regression vector (default = \code{NULL})
#' @param mu1 The dual tuning parameter used in the construction of the first projection direction (default = \code{NULL})
#' @param mu2 The dual tuning parameter used in the construction of the second projection direction (default = \code{NULL})
#' @param step1 The step size used to compute \code{mu1}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu1}
#' such that the dual optimization problem for constructing the first projection direction converges (default = \code{NULL})
#' @param step2 The step size used to compute \code{mu2}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu2}
#' such that the dual optimization problem for constructing the second projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu1} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu1} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis which claims that ITE is not above zero (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the ITE}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the ITE}
#' \item{decision}{\code{decision}\eqn{=1} implies the ITE is above zero \eqn{\newline}
#' \code{decision}\eqn{=0} implies the ITE is not above zero}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm na.omit
#' @importFrom scalreg scalreg
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlin}{SIHR}
#'
#' @examples
#' \donttest{
#' n1 <- 90
#' p <- 200
#' n2 <- 90
#' mu <- rep(0,p)
#' beta1 <- rep(0,p)
#' beta1[1:10] <- c(1:10)/5
#' beta2 <- rep(0,p)
#' beta2[1:5] <- c(1:5)/10
#' X1 <- MASS::mvrnorm(n1, mu, diag(p))
#' X2 <- MASS::mvrnorm(n2, mu, diag(p))
#' y1 <- X1%*%beta1 + rnorm(n1)
#' y2 <- X2%*%beta2 + rnorm(n2)
#' loading <- c(1,rep(0, (p-1)))
#' Est <- ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
#' }
ITE <- function(X1, y1, X2, y2, loading, intercept = TRUE, center = FALSE, init.Lasso1 = NULL, init.Lasso2 = NULL, lambda1 = NULL, lambda2 = NULL, mu1 = NULL, mu2 = NULL, step1 = NULL, step2 = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE){
  Est1 <- SIHR::LF(X1, y1, loading, intercept = intercept, center = center, init.Lasso = init.Lasso1, lambda = lambda1, mu = mu1, step = step1, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  Est2 <- SIHR::LF(X2, y2, loading, intercept = intercept, center = center, init.Lasso = init.Lasso2, lambda = lambda2, mu = mu2, step = step2, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  debias.est<-Est1$prop.est - Est2$prop.est
  se <- sqrt((Est1$se)^2 + (Est2$se)^2)
  CI <- c(debias.est - qnorm(1-alpha/2)*se, debias.est + qnorm(1-alpha/2)*se)
  if(debias.est - qnorm(1-alpha)*se > 0){
    dec <- 1
  }else{
    dec <- 0
  }
  returnList <- list("prop.est" = debias.est,
                     "se" = se,
                     "CI"=CI,
                     "decision" = dec
  )
  return(returnList)
}

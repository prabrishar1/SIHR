#' Confidence interval and hypothesis testing in high-dimensional generalized linear regression
#'
#' @description Constructs the confidence interval for the target quantity and test whether it is above \code{b0} or not
#' @param object an object of class \code{LF}, \code{ITE} or \code{QF} for which inference is desired
#' @param alpha Level of significance to test the null hypothesis
#' @param b0 The null value to be tested against
#'
#' @return
#' \item{CI}{The confidence interval for the target quantity}
#' \item{decision}{\code{decision}\eqn{=1} implies the target quantity is above \code{b0} \eqn{\newline}
#' \code{decision}\eqn{=0} implies the target quantity is above \code{b0}}
#'
#' @export
#'
#' @examples
#' n = 100
#' p = 400
#' A1gen <- function(rho,p){
#'   A1=matrix(0,p,p)
#'   for(i in 1:p){
#'     for(j in 1:p){
#'       A1[i,j]<-rho^(abs(i-j))
#'     }
#'   }
#'   A1
#' }
#' mu <- rep(0,p)
#' mu[1:5] <- c(1:5)/5
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- c(1:10)/5
#' X <- MASS::mvrnorm(n,mu,Cov)
#' y = X%*%beta + rnorm(n)
#' loading <- MASS::mvrnorm(1,rep(0,p),Cov)
#' Est = SIHR::GLM_LF(X = X, y = y, loading = loading)
#' CI = inf(Est)$CI
#' dec = inf(Est)$decision
inf <- function(object, alpha = 0.05, b0 = 0){
  if(class(object) == "QF"){
    CI <- matrix(NA,nrow=length(object$se),ncol=2)
    dec <- array(dim=1)
    for(i in 1:length(object$se)){
      CI[i,] <- c(object$prop.est - qnorm(1-alpha/2)*object$se[i], object$prop.est + qnorm(1-alpha/2)*object$se[i])
      if(object$prop.est - qnorm(1-alpha)*object$se[i] > b0){
        dec[i] <- 1
      } else {
        dec[i] <- 0
      }
    }
  } else {
    CI <- c(object$prop.est - qnorm(1-alpha/2)*object$se, object$prop.est + qnorm(1-alpha/2)*object$se)
    if(object$prop.est - qnorm(1-alpha)*object$se > 0){
      dec <- 1
    }else{
      dec <- 0
    }
  }
  returnList <- list("CI" = CI,
                     "decision" = dec)
  return(returnList)
}

#' Inference for difference of linear combinations of the regression vectors in high dimensional generalized linear regressions
#'
#' @description Computes the bias-corrected estimator of the difference of linearcombinations of the regression vectors for the high dimensional generalized linear regressions and the corresponding standard error.
#'
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_1}
#' @param model The high dimensional regression model, either \code{linear} or \code{logistic} or \code{logistic alternative} or \code{probit} or \code{inverse t1}
#' @param loading Loading, of length \eqn{p}
#' @param intercept.loading Should intercept be included for the \code{loading} (default = \code{TRUE})
#' @param intercept Should intercept(s) be fitted for the initial estimators (default = \code{TRUE})
#' @param init.coef1 Initial estimator of the first regression vector (default = \code{NULL})
#' @param init.coef2 Initial estimator of the second regression vector (default = \code{NULL})
#' @param lambda1 The tuning parameter in the construction of \code{init.coef1} (default = \code{NULL})
#' @param lambda2 The tuning parameter in the construction of \code{init.coef2} (default = \code{NULL})
#' @param mu1 The dual tuning parameter used in the construction of the first projection direction (default = \code{NULL})
#' @param mu2 The dual tuning parameter used in the construction of the second projection direction (default = \code{NULL})
#' @param step1 The step size used to compute \code{mu1}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu1}
#' such that the dual optimization problem for constructing the first projection direction converges (default = \code{NULL})
#' @param step2 The step size used to compute \code{mu2}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu2}
#' such that the dual optimization problem for constructing the second projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis which claims that ITE is not above zero (default = 0.05)         #removed
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the ITE}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the ITE}                                                         #removed
#' \item{decision}{\code{decision}\eqn{=1} implies the ITE is above zero \eqn{\newline}                   #removed
#' \code{decision}\eqn{=0} implies the ITE is not above zero}

#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm na.omit
#' @importFrom scalreg scalreg
#' @import CVXR Matrix glmnet
#'
#' @examples
#' n1 <- 90
#' p <- 200
#' n2 <- 90
#' mu <- rep(0,p)
#' beta1 <- rep(0,p)
#' beta1[1:10] <- c(1:10)/5
#' beta2 <- rep(0,p)
#' beta2[1:5] <- c(1:5)/10
#' set.seed(1203)
#' X1 <- MASS::mvrnorm(n1, mu, diag(p))
#' set.seed(1203)
#' X2 <- MASS::mvrnorm(n2, mu, diag(p))
#' set.seed(1203)
#' y1 <- X1%*%beta1 + rnorm(n1)
#' set.seed(1203)
#' y2 <- X2%*%beta2 + rnorm(n2)
#' loading <- c(1,rep(0, (p-1)))
#' Est <- ITE(X1 = X1, y1 = y1, X2 = X2, y2 = y2, model = "linear", loading = loading)
ITE <- function(X1, y1, X2, y2, model = "linear", loading, intercept.loading = TRUE, intercept = TRUE, init.coef1 = NULL, init.coef2 = NULL, lambda1 = NULL, lambda2 = NULL, mu1 = NULL, mu2 = NULL, step1 = NULL, step2 = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE){
  Est1 <- SIHR::GLM_LF(X=X1, y=y1, model = model, loading = loading, intercept.loading = intercept.loading, intercept = intercept, init.coef = init.coef1, lambda = lambda1, mu = mu1, step = step1, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  Est2 <- SIHR::GLM_LF(X=X2, y=y2, model = model, loading = loading, intercept.loading = intercept.loading, intercept = intercept, init.coef = init.coef2, lambda = lambda2, mu = mu2, step = step2, resol = resol, maxiter = maxiter, alpha = alpha, verbose = verbose)
  prop.est <- Est1$prop.est - Est2$prop.est
  se <- sqrt((Est1$se)^2 + (Est2$se)^2)
  CI <- c(prop.est - qnorm(1-alpha/2)*se, prop.est + qnorm(1-alpha/2)*se)
  if(prop.est - qnorm(1-alpha)*se > 0){
    dec <- 1
  }else{
    dec <- 0
  }
  #returnList <- list("prop.est" = prop.est,
  #                   "se"=se,
  #                   "CI"=CI,
  #                   "decision" = dec
  #)
  out <- list(prop.est = prop.est,
              se = se)
  structure(out, class = "ITE")
  #return(returnList)
}

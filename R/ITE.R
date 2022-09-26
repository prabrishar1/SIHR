#' Inference for difference of linear combinations of the regression vectors in
#' high dimensional generalized linear regressions
#' @description Computes the bias-corrected estimator of the difference of
#'   linearcombinations of the regression vectors for the high dimensional
#'   generalized linear regressions and the corresponding standard error.
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x
#'   \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x
#'   \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_1}
#' @param loading.mat Loading matrix, nrow=\eqn{p}, each column corresponds to a
#'   loading of interest
#' @param model The high dimensional regression model, either \code{"linear"}
#'   or \code{"logistic"} or \code{"logistic_alter"}
#' @param intercept Should intercept(s) be fitted for the initial estimators
#'   (default = \code{TRUE})
#' @param intercept.loading Should intercept term be included for the
#'   \code{loading} (default = \code{FALSE})
#' @param beta.init1 The initial estimator of the regression vector for the 1st
#'   data (default = \code{NULL})
#' @param beta.init2 The initial estimator of the regression vector for the 2nd
#'   data (default = \code{NULL})
#' @param lambda The tuning parameter in fitting initial model. If \code{NULL},
#'   it will be picked by cross-validation. (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the
#'   projection direction. If \code{NULL} it will be searched automatically.
#'   (default = \code{NULL})
#' @param prob.filter The threshold of estimated probabilities for filtering
#'   observations in logistic regression. (default = 0.05)
#' @param rescale The factor to enlarge the standard error to account for the
#'   finite sample bias. (default = 1.1)
#' @param alpha Level of significance to construct two-sided confidence interval
#'   (default = 0.05)
#' @param verbose Should intermediate message(s) be printed (default =
#'   \code{FALSE})
#'
#' @return A list consists of plugin estimators, debiased estimators, and confidence intervals.
#' For logistic regression, it also returns those items after probability transformation.
#' \item{est.plugin.vec}{The vector of plugin(biased) estimators for the
#' linear combination of regression coefficients, length of \code{ncol(loading.mat)};
#' corresponding to different column in \code{loading.mat}}
#' \item{est.debias.vec}{The vector of bias-corrected estimators for the linear
#' combination of regression coefficients, length of \code{ncol(loading.mat)};
#' corresponding to different column in \code{loading.mat}}
#' \item{se.vec}{The vector of standard errors of the bias-corrected estimators,
#' length of \code{ncol(loading.mat)}; corresponding to different column in
#' \code{loading.mat}}
#' \item{ci.mat}{The matrix of two.sided confidence interval for the linear
#' combination, dimension of \code{ncol(loading.mat)} x \eqn{2}; the row
#' corresponding to different column in \code{loading.mat}}
#' \item{prob.debias.vec}{The vector of bias-corrected estimators after probability
#' transformation, length of \code{ncol(loading.mat)}; corresponding to different
#' column in \code{loading.mat}.}
#' \item{prob.se.vec}{The vector of standard errors of the bias-corrected
#' estimators after probability transformation, length of \code{ncol(loading.mat)};
#' corresponding to different column in \code{loading.mat}.}
#' \item{prob.ci.mat}{The matrix of two.sided confidence interval of the bias-corrected
#' estimators after probability transformation, dimension of \code{ncol(loading.mat)} x \eqn{2};
#' the row corresponding to different column in \code{loading.mat}.}
#'
#' @export
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#'
#' @examples
#' X1 = matrix(rnorm(100*5), nrow=100, ncol=5)
#' y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
#' X2 = matrix(rnorm(90*5), nrow=90, ncol=5)
#' y2 = -0.4 + X2[,1] * 0.48 + X2[,2] * 1.1 + rnorm(90)
#' loading1 = c(1, 1, rep(0,3))
#' loading2 = c(-0.5, -1, rep(0,3))
#' loading.mat = cbind(loading1, loading2)
#' Est = ITE(X1, y1, X2, y2, loading.mat, model="linear")
#'
#' ## compute confidence intervals
#' ci(Est, alpha=0.05, alternative="two.sided")
#'
#' ## summary statistics
#' summary(Est)
ITE <- function(X1, y1, X2, y2, loading.mat, model=c("linear","logistic","logistic_alter"),
                intercept=TRUE, intercept.loading=FALSE, beta.init1=NULL, beta.init2=NULL, lambda=NULL, mu=NULL,
                prob.filter=0.05, rescale=1.1, alpha=0.05, verbose=FALSE){
  model = match.arg(model)
  if(verbose) cat(sprintf("Call: Inference for Linear Functional ======> Data 1/2 \n"))
  Est1 = LF(X1, y1, loading.mat, model, intercept, intercept.loading, beta.init1, lambda, mu, prob.filter, rescale, alpha, verbose)
  if(verbose) cat(sprintf("Call: Inference for Linear Functional ======> Data 2/2 \n"))
  Est2 = LF(X2, y2, loading.mat, model, intercept, intercept.loading, beta.init2, lambda, mu, prob.filter, rescale, alpha, verbose)
  est.plugin.vec = Est1$est.plugin.vec - Est2$est.plugin.vec
  est.debias.vec = Est1$est.debias.vec - Est2$est.debias.vec
  se.vec = sqrt((Est1$se.vec)^2 + (Est2$se.vec)^2)
  ci.mat <- cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  rownames(ci.mat) = paste("loading", 1:nrow(ci.mat), sep="")
  colnames(ci.mat) = c("lower","upper")

  ### works for probability transformation
  if(model %in% c("logistic", "logisitic_alter")){
    pred.fun = function(x) exp(x)/(1+exp(x))
    deriv.fun = function(x) exp(x)/(1+exp(x))^2
    prob.se.vec = sqrt((deriv.fun(Est1$est.debias.vec))^2 * (Est1$se.vec)^2 + (deriv.fun(Est2$est.debias.vec))^2 * (Est2$se.vec)^2)
    prob.debias.vec = pred.fun(Est1$est.debias.vec) - pred.fun(Est2$est.debias.vec)
    prob.ci.mat = cbind(prob.debias.vec - qnorm(1-alpha/2)*prob.se.vec,
                        prob.debias.vec + qnorm(1-alpha/2)*prob.se.vec)
    rownames(prob.ci.mat) = paste("loading", 1:nrow(ci.mat), sep="")
    colnames(prob.ci.mat) = c("lower","upper")
    obj <- list(est.plugin.vec = est.plugin.vec,
                est.debias.vec = est.debias.vec,
                se.vec         = se.vec,
                ci.mat         = ci.mat,
                prob.debias.vec = prob.debias.vec,
                prob.se.vec = prob.se.vec,
                prob.ci.mat = prob.ci.mat)
  }else{
    obj <- list(est.plugin.vec = est.plugin.vec,
                est.debias.vec = est.debias.vec,
                se.vec         = se.vec,
                ci.mat         = ci.mat)
  }

  class(obj) <- "ITE"
  obj
}

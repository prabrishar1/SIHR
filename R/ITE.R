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
#' @param loading.mat Loading matrix, nrow=\eqn{p}, each column corresponds to
#'   a loading of interest
#' @param model The high dimensional regression model, either \code{linear} or
#'   \code{logistic} or \code{logistic alternative} or \code{probit}
#' @param intercept Should intercept(s) be fitted for the initial estimators
#'   (default = \code{TRUE})
#' @param intercept.loading Should intercept be included for the \code{loading}
#'   (default = \code{TRUE})
#' @param lambda lambda The tuning parameter in fitting model (default =
#'   \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the
#'   projection direction (default = \code{NULL})
#' @param init.step The initial step size used to compute \code{mu}; if set to
#'   \code{NULL} it is computed to be the number of steps (\code{maxiter}) to
#'   obtain the smallest \code{mu}
#' @param resol The factor by which \code{mu} is increased/decreased to obtain
#'   the smallest \code{mu} such that the dual optimization problem for
#'   constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is
#'   increased/decreased to obtain the smallest \code{mu} such that the dual
#'   optimization problem for constructing the projection direction converges
#'   (default = 6)
#' @param alpha Level of significance to construct two-sided confidence interval
#'   (default = 0.05)
#' @param verbose Should intermediate message(s) be printed (default =
#'   \code{TRUE})
#'
#' @return
#' \item{est.plugin.vec}{The vector of plugin(biased) estimators for the
#'  linear combination of regression coefficients, length of
#'   \code{ncol(loading.mat)}; corresponding to different column in
#'    \code{loading.mat}}
#' \item{est.debias.vec}{The vector of bias-corrected
#'   estimators for the linear combination of regression coefficients, length of
#'   \code{ncol(loading.mat)}; corresponding to different column in
#'   \code{loading.mat}}
#' \item{se.vec}{The vector of standard errors of the
#'   bias-corrected estimators, length of \code{ncol(loading.mat)}; corresponding
#'   to different column in \code{loading.mat}}
#' \item{ci.mat}{The matrix of
#'   two.sided confidence interval for the linear combination, of dimension
#'   \code{ncol(loading.mat)} x \eqn{2}; the row corresponding to different column
#'   in \code{loading.mat}}
#'
#' @export
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm
#'
#' @examples
#' X1 = matrix(rnorm(100*120), nrow=100, ncol=120)
#' y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
#' X2 = matrix(rnorm(90*120), nrow=90, ncol=120)
#' y2 = -0.4 + X2[,1] * 0.48 + X2[,2] * 1.1 + rnorm(90)
#' loading.mat = cbind(c(1, 1, rep(0, 118), c(-0.5, -1, rep(0, 118))))
#' Est = ITE(X1, y1, X2, y2, loading.mat, model="linear")
#' Est$est.plugin.vec ## plugin(biased) estimators
#' Est$est.debias.vec ## bias-corrected estimators
#' Est$se.vec ## standard errors for bias-corrected estimators
#' Est$ci.mat ## two-sided confidence interval for bias-corrected estimators
#' \dontrun{
#' summary(Est)
#' }
ITE <- function(X1, y1, X2, y2, loading.mat, model="linear", intercept=TRUE, intercept.loading,
                lambda=NULL, mu=NULL, init.step=NULL, resol=1.5, maxiter=6, alpha=0.05,
                verbose=TRUE){
  if(verbose) cat(sprintf("Call: Inference for Linear Functional ======> Data 1/2 \n"))
  Est1 = LF(X1, y1, loading.mat, model, intercept, intercept.loading, lambda, mu, init.step,
            resol, maxiter, alpha, verbose)
  if(verbose) cat(sprintf("Call: Inference for Linear Functional ======> Data 2/2 \n"))
  Est2 = LF(X2, y2, loading.mat, model, intercept, intercept.loading, lambda, mu, init.step,
            resol, maxiter, alpha, verbose)
  est.plugin.vec = Est1$est.plugin.vec - Est2$est.plugin.vec
  est.debias.vec = Est1$est.debias.vec - Est2$est.debias.vec
  se.vec = sqrt((Est1$se)^2 + (Est2$se)^2)
  ci.mat <- cbind(est.vec - qnorm(1-alpha/2)*se.vec, est.vec + qnorm(1-alpha/2)*se.vec)
  rownames(ci.mat) = paste("loading", 1:nrow(ci.mat), sep="")
  colnames(ci.mat) = c("lower","upper")

  obj <- list(est.plugin.vec = est.plugin.vec,
              est.debias.vec = est.debias.vec,
              se.vec         = se.vec,
              ci.mat         = ci.mat)
  class(obj) <- "ITE"
  obj
}

#' Inference for weighted quadratic functional of difference of the regression vectors (excluding the intercept term) in
#' high dimensional generalized linear regressions.
#'
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_1}
#' @param G The set of indices, \code{G} in the quadratic form
#' @param A The matrix A in the quadratic form, of dimension \eqn{|G|\times}\eqn{|G|}. If \code{NULL} A would be set as the \eqn{|G|\times}\eqn{|G|} submatrix of the population covariance matrix corresponding to the index set \code{G} (default = \code{NULL})
#' @param model The high dimensional regression model, either \code{"linear"} or \code{"logistic"} or \code{"logistic_alter"}
#' @param intercept Should intercept(s) be fitted for the initial estimators (default = \code{TRUE})
#' @param beta.init1 The initial estimator of the regression vector for the 1st data (default = \code{NULL})
#' @param beta.init2 The initial estimator of the regression vector for the 2nd data (default = \code{NULL})
#' @param split Sampling splitting or not for computing the initial estimators.
#' It take effects only when \code{beta.init1 =  NULL} or \code{beta.init2 = NULL}. (default = \code{TRUE})
#' @param lambda The tuning parameter in fitting initial model. If \code{NULL}, it will be picked by cross-validation. (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction. If \code{NULL} it will be searched automatically. (default = \code{NULL})
#' @param prob.filter The threshold of estimated probabilities for filtering observations in logistic regression. (default = 0.05)
#' @param rescale The factor to enlarge the standard error to account for the finite sample bias. (default = 1.1)
#' @param tau The enlargement factor for asymptotic variance of the
#'   bias-corrected estimator to handle super-efficiency. It allows for a scalar
#'   or vector. (default = \code{c(0.25,0.5, 1)})
#' @param alpha Level of significance to construct two-sided confidence interval (default = 0.05)
#' @param verbose Should intermediate message(s) be printed, the projection
#'   direction be returned. (default = \code{FALSE})
#'
#' @return
#' \item{est.plugin}{The plugin(biased) estimator for the quadratic form of the regression vectors restricted to \code{G}}
#' \item{est.debias}{The bias-corrected estimator of the quadratic form of the regression vectors}
#' \item{se}{Standard errors of the bias-corrected estimator,
#' length of \code{tau}; corrsponding to different values of \code{tau}}
#' \item{ci.mat}{The matrix of two.sided confidence interval for the quadratic
#' form of the regression vector; row corresponds to different values of
#' \code{tau}}
#' \item{proj1}{The projection direction for the first sample. It will be returned only if \code{verbose} set as TRUE}
#' \item{proj2}{The projection direction for the second sample. It will be returned only if \code{verbose} set as TRUE}
#' @export
#'
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#' @examples
#' X1 = matrix(rnorm(100*5), nrow=100, ncol=5)
#' y1 = -0.5 + X1[,1] * 0.5 + X1[,2] * 1 + rnorm(100)
#' X2 = matrix(rnorm(90*5), nrow=90, ncol=5)
#' y2 = -0.4 + X2[,1] * 0.48 + X2[,2] * 1.1 + rnorm(90)
#' G = c(1,2)
#' A = matrix(c(1.5, 0.8, 0.8, 1.5), nrow=2, ncol=2)
#' Est = Dist(X1, y1, X2, y2, G, A, model="linear")
#' Est$ci

Dist <- function(X1, y1, X2, y2, G, A= NULL, model = c("linear","logistic","logistic_alter"), intercept = TRUE, beta.init1 = NULL, beta.init2 = NULL,
                 split=TRUE, lambda = NULL, mu = NULL, prob.filter = 0.05, rescale = 1.1, tau=c(0.25, 0.5, 1), alpha = 0.05, verbose=FALSE){
  model = match.arg(model)
  X1 = as.matrix(X1)
  y1 = as.vector(y1)
  X2 = as.matrix(X2)
  y2 = as.vector(y2)
  p = ncol(X1) + as.integer(intercept)

  G = sort(as.vector(G))
  nullA = ifelse(is.null(A), TRUE, FALSE)

  ## check arguments
  check.args.Dist(X1 = X1, y1 = y1, X2 = X2, y2 = y2, G = G, A = A, model = model, intercept = intercept, beta.init1 = beta.init1, beta.init2 = beta.init2,
                  split = split, lambda = lambda, mu = mu, prob.filter = prob.filter, rescale = rescale, tau = tau, alpha = alpha, verbose=verbose)

  ### specify relevant functions ###
  funs.all = relevant.funs(intercept=intercept, model=model)
  train.fun = funs.all$train.fun

  ### centralize X ###
  X1 = scale(X1, center=TRUE, scale=F)
  X2 = scale(X2, center=TRUE, scale=F)

  ### Initial lasso estimator of beta ###
  if(is.null(beta.init1)){
    if(split){
      idx1 = sample(1:nrow(X1), size=round(nrow(X1)/2), replace=F)
      idx2 = setdiff(1:nrow(X1), idx1)
      X1.init = X1[idx1,,drop=F]; y1.init = y1[idx1]
      X1 = X1[idx2,,drop=F]; y1 = y1[idx2]
    }else{
      X1.init = X1; y1.init = y1
    }
    beta.init1 = train.fun(X1.init, y1.init, lambda=lambda)$lasso.est
  }
  beta.init1 = as.vector(beta.init1)

  if(is.null(beta.init2)){
    if(split){
      idx1 = sample(1:nrow(X2), size=round(nrow(X2)/2), replace=F)
      idx2 = setdiff(1:nrow(X2), idx1)
      X2.init = X2[idx1,,drop=F]; y2.init = y2[idx1]
      X2 = X2[idx2,,drop=F]; y2 = y2[idx2]
    }else{
      X2.init = X2; y2.init = y2
    }
    beta.init2 = train.fun(X2.init, y2.init, lambda=lambda)$lasso.est
  }
  beta.init2 = as.vector(beta.init2)

  n.min = min(nrow(X1), nrow(X2))
  sparsity = max(sum(abs(beta.init1)>1e-4),sum(abs(beta.init2)>1e-4))
  gamma.init = beta.init2-beta.init1

  ### Adjust A and loading ###
  if(intercept) G = G+1
  if(nullA){
    ## if A is not specified, we assume that X1 and X2 have the same covariates dist
    X = rbind(X1,X2)
    if(intercept) X = cbind(1,X)
    A = t(X)%*%X/nrow(X)
    A = A[G,G,drop=F]
  }
  loading = rep(0,p)
  loading[G] = A%*%gamma.init[G]
  if(intercept) loading = loading[-1]

  ### Run LF twice ###
  Est1 <- LF(X1, y1, loading.mat = loading, model = model, intercept = intercept, intercept.loading = FALSE, beta.init = beta.init1, lambda = lambda, mu = mu, prob.filter = prob.filter, rescale = rescale, alpha = alpha, verbose = verbose)
  Est2 <- LF(X2, y2, loading.mat = loading, model = model, intercept = intercept, intercept.loading = FALSE, beta.init = beta.init2, lambda = lambda, mu = mu, prob.filter = prob.filter, rescale = rescale, alpha = alpha, verbose = verbose)

  est.plugin <- as.numeric(t(gamma.init[G])%*%A%*%gamma.init[G])
  est.debias <- est.plugin + 2*(Est2$est.debias.vec - Est2$est.plugin.vec) - 2*(Est1$est.debias.vec - Est1$est.plugin.vec)

  V.base = 4*Est1$se.vec^2 + 4*Est2$se.vec^2
  if(nullA){
    V.A = sum((as.vector((X[,G,drop=F]%*%gamma.init[G])^2) -
                 as.numeric(t(gamma.init[G]) %*% A %*% gamma.init[G]))^2) /(nrow(X))^2
  }else{
    V.A = 0
  }
  if(model == 'linear'){
    se.add = tau*(1/sqrt(n.min))
  }else{
    se.add = tau*max(1/sqrt(n.min), sparsity*log(p)/n.min)
  }
  se = sqrt(V.base + V.A) + se.add

  ci.mat = cbind(pmax(est.debias - qnorm(1-alpha/2)*se,0), pmax(est.debias + qnorm(1-alpha/2)*se,0))
  colnames(ci.mat) = c("lower", "upper")
  rownames(ci.mat) = paste0('tau',tau)

  if(verbose){
    obj <- list(est.plugin = est.plugin,
                est.debias = est.debias,
                se         = se,
                ci.mat     = ci.mat,
                tau        = tau,
                proj1      = Est1$proj,
                proj2      = Est2$proj)
  }else{
    obj <- list(est.plugin = est.plugin,
                est.debias = est.debias,
                se         = se,
                ci.mat     = ci.mat,
                tau        = tau)
  }

  class(obj) = "Dist"
  return(obj)
}

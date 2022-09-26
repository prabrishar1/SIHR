#' Inference for quadratic forms of the regression vector in high dimensional
#' generalized linear regressions
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param G The set of indices, \code{G} in the quadratic form
#' @param A The matrix A in the quadratic form, of dimension
#'   \eqn{|G|\times}\eqn{|G|}. If \code{NULL} A would be set as the
#'   \eqn{|G|\times}\eqn{|G|} submatrix of the population covariance matrix
#'   corresponding to the index set \code{G} (default = \code{NULL})
#' @param model The high dimensional regression model, either \code{"linear"}
#'   or \code{"logistic"} or \code{"logistic_alter"}
#' @param intercept Should intercept be fitted for the initial estimator
#'   (default = \code{TRUE})
#' @param beta.init The initial estimator of the regression vector (default =
#'   \code{NULL})
#' @param split Sampling splitting or not for computing the initial estimator.
#'   It take effects only when \code{beta.init =  NULL}. (default = \code{TRUE})
#' @param lambda The tuning parameter in fitting initial model. If \code{NULL},
#'   it will be picked by cross-validation. (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the
#'   projection direction. If \code{NULL} it will be searched automatically.
#'   (default = \code{NULL})
#' @param prob.filter The threshold of estimated probabilities for filtering
#'   observations in logistic regression. (default = 0.05)
#' @param rescale The factor to enlarge the standard error to account for the
#'   finite sample bias. (default = 1.1)
#' @param tau The enlargement factor for asymptotic variance of the
#'   bias-corrected estimator to handle super-efficiency. It allows for a scalar
#'   or vector. (default = \code{c(0.25,0.5)})
#' @param alpha Level of significance to construct two-sided confidence interval
#'   (default = 0.05)
#' @param verbose Should intermediate message(s) be printed, the projection
#'   direction be returned. (default = \code{FALSE})
#'
#' @return
#' \item{est.plugin}{The plugin(biased) estimator for the quadratic form of the
#' regression vector restricted to \code{G}}
#' \item{est.debias}{The bias-corrected estimator of the quadratic form of the
#' regression vector}
#' \item{se}{Standard errors of the bias-corrected estimator,
#' length of \code{tau}; corrsponding to different values of \code{tau}}
#' \item{ci.mat}{The matrix of two.sided confidence interval for the quadratic
#' form of the regression vector; row corresponds to different values of
#' \code{tau}}
#' \item{proj}{The projection direction. It will be returned only if \code{verbose} set as TRUE}
#' @export
#'
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#' @examples
#' X = matrix(rnorm(100*5), nrow=100, ncol=5)
#' y = X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
#' G = c(1,2)
#' A = matrix(c(1.5, 0.8, 0.8, 1.5), nrow=2, ncol=2)
#' Est = QF(X, y, G, A, model="linear")
#' ## compute confidence intervals
#' ci(Est, alpha=0.05, alternative="two.sided")
#'
#' ## summary statistics
#' summary(Est)
QF <- function(X, y, G, A=NULL, model=c("linear","logistic","logistic_alter"),
               intercept=TRUE, beta.init=NULL, split=TRUE, lambda=NULL, mu=NULL,
               prob.filter=0.05, rescale=1.1, tau=c(0.25, 0.5), alpha=0.05, verbose=FALSE){
  model = match.arg(model)
  X = as.matrix(X)
  y = as.vector(y)
  G = sort(as.vector(G))
  nullA = ifelse(is.null(A), TRUE, FALSE)

  ### check arguments ###
  check.args.QF(X=X, y=y, G=G, A=A, model=model, intercept=intercept, beta.init=beta.init,
                split=split, lambda=lambda, mu=mu, prob.filter=prob.filter,
                rescale=rescale, tau=tau, alpha=alpha, verbose=verbose)

  ### specify relevant functions ###
  funs.all = relevant.funs(intercept=intercept, model=model)
  train.fun = funs.all$train.fun
  pred.fun = funs.all$pred.fun
  deriv.fun = funs.all$deriv.fun
  weight.fun = funs.all$weight.fun
  cond_var.fun = funs.all$cond_var.fun

  ### centralize X ###
  X = scale(X, center=TRUE, scale=F)

  if(is.null(beta.init)){
    ### split data ###
    if(split){
      idx1 = sample(1:nrow(X),size=round(nrow(X)/2), replace = F)
      idx2 = setdiff(1:nrow(X), idx1)
      X1 = X[idx1,]; y1 = y[idx1]
      X = X[idx2,]; y = y[idx2]
    }else{
      X1 = X; y1 = y
    }
    beta.init = train.fun(X1, y1, lambda=lambda)$lasso.est
  }
  beta.init = as.vector(beta.init)
  sparsity = sum(abs(beta.init)>1e-4)

  ### prepare values ###
  if(intercept) X = cbind(1, X)
  n = nrow(X); p = ncol(X)
  pred = as.vector(pred.fun(X%*%beta.init))
  deriv = as.vector(deriv.fun(X%*%beta.init))
  weight = as.vector(weight.fun(X%*%beta.init))
  cond_var = as.vector(cond_var.fun(pred, y, sparsity))

  ### prob.filter ###
  if(model!='linear'){
    idx = as.logical((pred>prob.filter)*(pred <(1-prob.filter)))
    if(mean(idx) < 0.8) warning("More than 20 % observations are filtered out
                                as their estimated probabilities are too close to the
                                boundary 0 or 1.")
  }else{
    idx = rep(TRUE, n)
  }
  X.filter = X[idx,]
  y.filter = y[idx]
  weight.filter = weight[idx]
  deriv.filter = deriv[idx]
  n.filter = nrow(X.filter)

  ### Adjust A and loading ###
  if(intercept) G = G+1
  if(nullA){
    A = t(X)%*%X / nrow(X)
    A = A[G,G,drop=F]
  }
  loading = rep(0, p)
  loading[G] = A%*%beta.init[G]
  loading.norm = sqrt(sum(loading^2))

  ############### Function of Computing Direction #################
  compute_direction = function(loading, X, weight, deriv, n){
    if(verbose) cat("Computing QF... \n")
    ### loading.norm too small, direction is set as 0 ###
    if(loading.norm <= 1e-5){
      cat("loading norm is too small, set projection direction as numeric(0) \n")
      direction = rep(0, length(loading))
    }
    ### loading.norm large enough ###
    if(loading.norm > 1e-5){
      if (n >= 6*p){
        temp = sqrt(weight*deriv)*X
        Sigma.hat = t(temp)%*%temp / n
        direction = solve(Sigma.hat) %*% loading / loading.norm
      }else{
        ### CVXR sometimes break down accidentally, we catch the error and offer an alternative ###
        direction_alter = FALSE
        tryCatch(
          expr = {
            ### cases when no mu specified ###
            if(is.null(mu)){
              if(n >= 0.9*p){
                step.vec = incr.vec = rep(NA, 3)
                for(t in 1:3){
                  index.sel = sample(1:n, size=round(0.9*p), replace=FALSE)
                  Direction.Est.temp = Direction_searchtuning(X[index.sel,], loading, weight=weight[index.sel], deriv= deriv[index.sel])
                  step.vec[t] = Direction.Est.temp$step
                  incr.vec[t] = Direction.Est.temp$incr
                }
                step = getmode(step.vec)
                incr = getmode(incr.vec)
                Direction.Est = Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, step=step, incr=incr)
                while(Direction.Est$status!='optimal'){
                  step = step + incr
                  Direction.Est =  Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, step=step, incr=incr)
                }
                if(verbose) cat(paste0('The projection direction is identified at mu = ', round(Direction.Est$mu, 6),
                                       'at step =',step ,'\n'))
              }else{
                Direction.Est = Direction_searchtuning(X, loading, weight=weight, deriv=deriv)
                if(verbose) cat(paste0('The projection direction is identified at mu = ', round(Direction.Est$mu, 6),
                                       'at step =',Direction.Est$step ,'\n'))
              }
            }
            ### cases when mu specified ###
            if(!is.null(mu)){
              Direction.Est = Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, mu=mu)
              while(Direction.Est$status!='optimal'){
                mu = mu*1.5
                Direction.Est = Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, mu=mu)
              }
              if(verbose) cat(paste0('The projection direction is identified at mu = ', round(Direction.Est$mu, 6), '\n'))
            }
            direction = Direction.Est$proj
          },
          error = function(e){
            message("Caught an error in CVXR during computing direction, switch to the alternative method")
            print(e)
            direction_alter <<- TRUE
          }
        )
        if(direction_alter){
          temp = sqrt(weight*deriv)*X
          Sigma.hat = t(temp)%*%temp / n
          Sigma.hat.inv = diag(1/diag(Sigma.hat))
          direction = Sigma.hat.inv %*% loading / loading.norm
        }
      }
    }
    return(direction)
  }

  ################# Correction Direction ####################
  direction = compute_direction(loading, X.filter, weight.filter, deriv.filter, n.filter)

  ################# Bias Correction ################
  est.plugin = as.numeric(t(beta.init[G]) %*% A %*% beta.init[G])
  correction = 2 * mean((weight * (y-pred) * X) %*% direction)
  est.debias = max(as.numeric(est.plugin + correction * loading.norm), 0)

  ############## Compute SE and Construct CI ###############
  V.base = 4 * sum(((sqrt(weight^2 * cond_var) * X) %*% direction)^2)/n * loading.norm^2
  if((n>0.9*p)&(model=='linear')) V.base = V.base else V.base = rescale^2 * V.base
  if(nullA){
    V.A = sum((as.vector((X[,G,drop=F]%*%beta.init[G])^2) -
                 as.numeric(t(beta.init[G]) %*% A %*% beta.init[G]))^2) / n
  }else{
    V.A = 0
  }
  V = (V.base + V.A)
  if(model=='linear'){
    se.add = tau*(1/sqrt(n))
  }else{
    se.add = tau*max(1/sqrt(n), sparsity*log(p)/n)
  }
  se = sqrt(V/n) + se.add

  ci.mat = cbind(pmax(est.debias - qnorm(1-alpha/2)*se,0), pmax(est.debias + qnorm(1-alpha/2)*se,0))
  colnames(ci.mat) = c("lower", "upper")
  rownames(ci.mat) = paste0('tau',tau)
  if(verbose){
    obj <- list(est.plugin = est.plugin,
                est.debias = est.debias,
                se         = se,
                ci.mat     = ci.mat,
                tau        = tau,
                proj       = direction * loading.norm)
  }else{
    obj <- list(est.plugin = est.plugin,
                est.debias = est.debias,
                se         = se,
                ci.mat     = ci.mat,
                tau        = tau)
  }

  class(obj) = "QF"
  obj
}

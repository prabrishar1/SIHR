#' Inference for linear combination of the regression vector in high dimensional
#' generalized linear regression
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading.mat Loading matrix, nrow=\eqn{p}, each column corresponds to a
#'   loading of interest
#' @param model The high dimensional regression model, either \code{"linear"}
#'   or \code{"logistic"} or \code{"logistic_alter"}
#' @param intercept Should intercept be fitted for the initial estimator
#'   (default = \code{TRUE})
#' @param intercept.loading Should intercept term be included for the loading
#'   (default = \code{FALSE})
#' @param beta.init The initial estimator of the regression vector (default =
#'   \code{NULL})
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
#' @param verbose Should intermediate message(s) be printed, the projection
#'   direction be returned. (default = \code{FALSE})
#'
#' @return
#' \item{est.plugin.vec}{The vector of plugin(biased) estimators for the
#' linear combination of regression coefficients, length of
#' \code{ncol(loading.mat)}; each corresponding to a loading of interest}
#' \item{est.debias.vec}{The vector of bias-corrected estimators for the linear
#' combination of regression coefficients, length of \code{ncol(loading.mat)};
#' each corresponding to a loading of interest}
#' \item{se.vec}{The vector of standard errors of the bias-corrected estimators,
#' length of \code{ncol(loading.mat)}; each corresponding to a loading of interest}
#' \item{ci.mat}{The matrix of two.sided confidence interval for the linear
#' combination, of dimension \code{ncol(loading.mat)} x \eqn{2}; each row
#' corresponding to a loading of interest}
#' \item{proj.mat}{The matrix of projection directions; each column corresponding
#' to a loading of interest. It will be returned only if \code{verbose} set as TRUE}
#'
#' @export
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#'
#' @examples
#' X = matrix(rnorm(100*5), nrow=100, ncol=5)
#' y = -0.5 + X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
#' loading1 = c(1, 1, rep(0, 3))
#' loading2 = c(-0.5, -1, rep(0, 3))
#' loading.mat = cbind(loading1, loading2)
#' Est = LF(X, y, loading.mat, model="linear")
#'
#' ## compute confidence intervals
#' ci(Est, alpha=0.05, alternative="two.sided")
#'
#' ## summary statistics
#' summary(Est)
LF <- function(X, y, loading.mat, model=c("linear","logistic","logistic_alter"),
               intercept=TRUE, intercept.loading=FALSE, beta.init=NULL, lambda=NULL,
               mu=NULL, prob.filter=0.05, rescale=1.1, alpha=0.05, verbose=FALSE){
  model = match.arg(model)
  X = as.matrix(X)
  y = as.vector(y)
  loading.mat = as.matrix(loading.mat)

  ### Check arguments ###
  if(!is.logical(verbose)) verbose=TRUE
  if(intercept==FALSE && intercept.loading==TRUE){
    intercept.loading = FALSE
    cat("Argument 'intercept.loading' is set to FALSE, because 'intercept' is FALSE \n")
  }
  check.args.LF(X=X, y=y, loading.mat=loading.mat, model=model, intercept=intercept,
                intercept.loading=intercept.loading, beta.init=beta.init, lambda=lambda,
                mu=mu, rescale=rescale, prob.filter=prob.filter, alpha=alpha,
                verbose=verbose)

  ### specify relevant functions ###
  funs.all = relevant.funs(intercept=intercept, model=model)
  train.fun = funs.all$train.fun
  pred.fun = funs.all$pred.fun
  deriv.fun = funs.all$deriv.fun
  weight.fun = funs.all$weight.fun
  cond_var.fun = funs.all$cond_var.fun

  ### centralize X ###
  X_means = colMeans(X)
  X = scale(X, center=TRUE, scale=F)

  ### Initial lasso estimator of beta ###
  if(is.null(beta.init)) beta.init = train.fun(X, y, lambda=lambda)$lasso.est
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

  ### storing infos ###
  n.loading = ncol(loading.mat)
  est.plugin.vec = rep(NA, n.loading)
  est.debias.vec = rep(NA, n.loading)
  se.vec = rep(NA, n.loading)
  ci.mat = matrix(NA, nrow = n.loading, ncol = 2)
  colnames(ci.mat) = c("lower","upper"); rownames(ci.mat) = paste("loading",1:n.loading,sep="")
  proj.mat = matrix(NA, nrow=p, ncol=n.loading)

  ############### Function of Computing Direction #################
  compute_direction <- function(loading, X, weight, deriv, n){
    if(verbose) cat(sprintf("Computing LF for loading (%i/%i)... \n", i.loading, n.loading))
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
      } else {
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
  ############### Filtering ###############
  for(i.loading in 1:n.loading){
    ### adjust loading ###
    loading = as.vector(loading.mat[,i.loading])
    if(intercept){
      if(intercept.loading){
        loading = loading - X_means
        loading = c(1, loading)
      }else{
        loading = c(0, loading)
      }
    }
    loading.norm = sqrt(sum(loading^2))

    ############## Correction Direction ###############
    direction = compute_direction(loading, X.filter, weight.filter, deriv.filter, n.filter)

    ############## Bias Correction ###############
    est.plugin = sum(beta.init * loading)
    correction = mean((weight * (y-pred) * X) %*% direction)
    est.debias = est.plugin + correction*loading.norm

    ############## Compute SE and Construct CI ###############
    V = sum(((sqrt(weight^2 * cond_var) * X) %*% direction)^2)/n * loading.norm^2
    if((n>0.9*p)&(model=='linear')) se = sqrt(V/n) else se = rescale*sqrt(V/n)
    ci = c(est.debias - qnorm(1-alpha/2)*se, est.debias + qnorm(1-alpha/2)*se)

    ############## Store Infos ###############
    est.plugin.vec[i.loading] = est.plugin
    est.debias.vec[i.loading] = est.debias
    se.vec[i.loading] = se
    ci.mat[i.loading, ] = ci
    proj.mat[, i.loading] = direction * loading.norm
  }

  if(verbose){
    obj <- list(est.plugin.vec = est.plugin.vec,
                est.debias.vec = est.debias.vec,
                se.vec         = se.vec,
                ci.mat         = ci.mat,
                proj.mat       = proj.mat)
  }else{
    obj <- list(est.plugin.vec = est.plugin.vec,
                est.debias.vec = est.debias.vec,
                se.vec         = se.vec,
                ci.mat         = ci.mat)
  }

  class(obj) = "LF"
  obj
}


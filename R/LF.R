#' Inference for linear combination of the regression vector in high dimensional
#' generalized linear regression
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading.mat Loading matrix, nrow=\eqn{p}, each column corresponds to
#'   a loading of interest
#' @param model The high dimensional regression model, either \code{linear} or
#'   \code{logistic} or \code{logistic_alternative} or \code{probit}
#' @param intercept Should intercept be fitted for the initial estimator
#'   (default = \code{TRUE})
#' @param intercept.loading Should intercept be included for the loading
#'   (default = \code{FALSE})
#' @param beta.init The initial estimator of the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter in fitting model (default = \code{NULL})
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
#' @param alpha Level of significance to construct two-sided confidence interval (default = 0.05)
#' @param verbose Should intermediate message(s) be printed (default =
#'   \code{TRUE})
#'
#' @return
#' \item{est.plugin.vec}{The vector of plugin(biased) estimators for the
#'   linear combination of regression coefficients, length of
#'   \code{ncol(loading.mat)}; each corresponding to a loading of interest}
#' \item{est.debias.vec}{The vector of bias-corrected
#'   estimators for the linear combination of regression coefficients, length of
#'   \code{ncol(loading.mat)}; each corresponding to a loading of interest}
#' \item{se.vec}{The vector of standard errors of the
#'   bias-corrected estimators, length of \code{ncol(loading.mat)}; each
#'   corresponding to a loading of interest}
#' \item{ci.mat}{The matrix of
#'   two.sided confidence interval for the linear combination, of dimension
#'   \code{ncol(loading.mat)} x \eqn{2}; each row corresponding to a loading of
#'   interest}
#' \item{proj.mat}{The matrix of projection directions; each column corresponding
#'   to a loading of interest}
#'
#' @export
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#'
#' @examples
#' X = matrix(rnorm(100*10), nrow=100, ncol=10)
#' y = -0.5 + X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
#' loading1 = c(1, 1, rep(0, 8))
#' loading2 = c(-0.5, -1, rep(0, 8))
#' loading.mat = cbind(loading1, loading2)
#' Est = LF(X, y, loading.mat, model="linear")
#'
#' ## compute confidence intervals
#' ci(Est, alpha=0.05, alternative="two.sided")
#'
#' ## summary statistics
#' summary(Est)
LF <- function(X, y, loading.mat, model=c("linear","logistic","logistic_alternative","probit"),
               intercept=TRUE, intercept.loading=FALSE, beta.init=NULL, lambda=NULL,
               mu=NULL, init.step=NULL, resol=1.5, maxiter=6, alpha=0.05,
               verbose=TRUE){
  model = match.arg(model)
  X = as.matrix(X)
  y = as.vector(y)
  loading.mat = as.matrix(loading.mat)
  nullmu = ifelse(is.null(mu), TRUE, FALSE)
  null_initstep = ifelse(is.null(init.step), TRUE, FALSE)

  ### Check arguments ###
  if(!is.logical(verbose)) verbose=TRUE
  if(intercept==FALSE && intercept.loading==TRUE){
    intercept.loading = FALSE
    cat("Argument 'intercept.loading' is set to FALSE, because 'intercept' is FALSE")
  }
  check.args.LF(X=X, y=y, loading.mat=loading.mat, model=model, intercept=intercept,
                intercept.loading=intercept.loading, beta.init=beta.init, lambda=lambda,
                mu=mu, init.step=init.step, resol=resol, maxiter=maxiter,
                alpha=alpha, verbose=verbose)

  ### specify relevant functions ###
  funs.all = relevant.funs(intercept=intercept, model=model)
  train.fun = funs.all$train.fun
  pred.fun = funs.all$pred.fun
  deriv.fun = funs.all$deriv.fun
  weight.fun = funs.all$deriv.fun
  cond_var.fun = funs.all$cond_var.fun

  ### centralize X ###
  X_means = colMeans(X)
  X = scale(X, center=TRUE, scale=F)

  ### Initial lasso estimator of beta ###
  if(is.null(beta.init)) beta.init = train.fun(X, y, lambda=lambda)$lasso.est
  beta.init = as.vector(beta.init)

  ### prepare values ###
  if(intercept) X = cbind(1, X)
  n = nrow(X); p = ncol(X)
  pred = as.vector(pred.fun(X%*%beta.init))
  deriv = as.vector(deriv.fun(X%*%beta.init))
  weight = as.vector(weight.fun(X%*%beta.init))
  cond_var = as.vector(cond_var.fun(pred, y))

  ### storing infos ###
  n.loading = ncol(loading.mat)
  est.plugin.vec = rep(NA, n.loading)
  est.debias.vec = rep(NA, n.loading)
  se.vec = rep(NA, n.loading)
  ci.mat = matrix(NA, nrow = n.loading, ncol = 2)
  colnames(ci.mat) = c("lower","upper"); rownames(ci.mat) = paste("loading",1:n.loading,sep="")
  proj.mat = matrix(NA, nrow=p, ncol=n.loading)

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

    ############### Correction Direction #################
    if(verbose) cat(sprintf("Computing LF for loading (%i/%i)... \n", i.loading, n.loading))
    if (n >= 6*p){
      temp = weight*deriv*X
      Sigma.hat = t(temp)%*%temp / n
      direction = solve(Sigma.hat) %*% loading / loading.norm
    } else {
      ### CVXR sometimes break down accidentally, we catch the error and offer an alternative ###
      direction_alter = FALSE
      tryCatch(
        expr = {
          ### find init.step ###
          if(null_initstep){
            step.vec <- rep(NA,3)
            for(t in 1:3){
              index.sel <- sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
              Direction.Est.temp <-  Direction_searchtuning(X[index.sel,], loading, weight = weight[index.sel], deriv.vec = deriv[index.sel], resol, maxiter)
              step.vec[t] <- Direction.Est.temp$step
            }
            init.step<- getmode(step.vec)
          }
          ### for loop to find direction ###
          # if(verbose) cat(sprintf("---> Initial step set as: %s \n", init.step))
          for(step in init.step:1){
            # if(verbose) cat(sprintf("---> Finding Direction with step: %s \n", step))
            if(nullmu) mu = sqrt(2.01*log(p)/n)*resol^{-(step-1)}
            Direction.Est <-  Direction_fixedtuning(X, loading, mu = mu, weight = weight, deriv.vec = deriv)
            if(is.na(Direction.Est) || length(Direction.Est$proj)==0){
              next
            }else{
              if(verbose) cat(sprintf("---> Direction is identified at step: %s \n", step))
              direction <- Direction.Est$proj
              break
            }
          }
        },
        error = function(e){
          message("Caught an error in CVXR during computing direction")
          print(e)
          direction_alter <<- TRUE
        },
        warning = function(w){
          message("Caught a warning in CVXR during computing direction")
          print(w)
          direction_alter <<- TRUE
        }
      )
      if(direction_alter){
        temp = weight*deriv*X
        Sigma.hat = t(temp)%*%temp / n
        Sigma.hat.inv = diag(1/diag(Sigma.hat))
        direction = Sigma.hat.inv %*% loading / loading.norm
      }
    }

    ############## Bias Correction ###############
    est.plugin = sum(beta.init * loading)
    correction = mean((weight * (y-pred) * X) %*% direction)
    est.debias = est.plugin + correction*loading.norm

    ############## Compute SE and Construct CI ###############
    V = sum(((sqrt(weight^2 * cond_var) * X) %*% direction)^2)/n * loading.norm^2
    se = sqrt(V/n)
    ci = c(est.debias - qnorm(1-alpha/2)*se, est.debias + qnorm(1-alpha/2)*se)

    ############## Store Infos ###############
    est.plugin.vec[i.loading] = est.plugin
    est.debias.vec[i.loading] = est.debias
    se.vec[i.loading] = se
    ci.mat[i.loading, ] = ci
    proj.mat[, i.loading] = direction * loading.norm
  }

  obj <- list(est.plugin.vec = est.plugin.vec,
              est.debias.vec = est.debias.vec,
              se.vec         = se.vec,
              ci.mat         = ci.mat,
              proj.mat       = proj.mat)
  class(obj) = "LF"
  obj
}


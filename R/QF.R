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
#'   or \code{"logistic"} or \code{"logistic_alternative"} or \code{"probit"}
#' @param intercept Should intercept be fitted for the initial estimator
#'   (default = \code{TRUE})
#' @param tau.vec The vector of enlargement factors for asymptotic variance of
#'   the bias-corrected estimator to handle super-efficiency (default =
#'   \eqn{c(0, 0.5, 1)})
#' @param beta.init The initial estimator of the regression vector (default =
#'   \code{NULL})
#' @param lambda The tuning parameter in fitting initial model. If \code{NULL},
#'   it will be picked by cross-validation. (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the
#'   projection direction. If \code{NULL} it will be searched automatically.
#'   (default = \code{NULL})
#' @param alpha Level of significance to construct two-sided confidence interval
#'   (default = 0.05)
#' @param verbose Should intermediate message(s) be printed (default =
#'   \code{TRUE})
#'
#' @return
#' \item{est.plugin}{The plugin(biased) estimator for the quadratic form of the
#' regression vector restricted to \code{G}}
#' \item{est.debias}{The bias-corrected estimator of the quadratic form of the
#' regression vector}
#' \item{se.vec}{The vector of standard errors of the bias-corrected estimator,
#' length of \code{tau.vec}; corrsponding to different values of \code{tau.vec}}
#' \item{ci.mat}{The matrix of two.sided confidence interval for the quadratic
#' form of the regression vector; row corresponds to different values of \code{tau.vec}}
#' \item{proj}{The projection direction}
#' @export
#'
#' @import CVXR glmnet
#' @importFrom stats coef dnorm median pnorm qnorm symnum
#' @examples
#' X = matrix(rnorm(100*10), nrow=100, ncol=10)
#' y = X[,1] * 0.5 + X[,2] * 1 + rnorm(100)
#' G = c(1,2)
#' A = matrix(c(1.5, 0.8, 0.8, 1.5), nrow=2, ncol=2)
#' Est = QF(X, y, G, A, model="linear")
#' ## compute confidence intervals
#' ci(Est, alpha=0.05, alternative="two.sided")
#'
#' ## summary statistics
#' summary(Est)
QF <- function(X, y, G, A=NULL, model=c("linear","logistic","logistic_alternative","probit"),
               intercept=TRUE, tau.vec=c(0, 0.5, 1), beta.init=NULL, lambda=NULL, mu=NULL,
               alpha=0.05, verbose=TRUE){
  model = match.arg(model)
  X = as.matrix(X)
  y = as.vector(y)
  G = sort(as.vector(G))
  nullA = ifelse(is.null(A), TRUE, FALSE)

  ### check arguments ###
  check.args.QF(X=X, y=y, G=G, A=A, model=model, intercept=intercept, tau.vec=tau.vec,
                beta.init=beta.init, lambda=lambda, mu=mu, alpha=alpha, verbose=verbose)

  ### specify relevant functions ###
  funs.all = relevant.funs(intercept=intercept, model=model)
  train.fun = funs.all$train.fun
  pred.fun = funs.all$pred.fun
  deriv.fun = funs.all$deriv.fun
  weight.fun = funs.all$weight.fun
  cond_var.fun = funs.all$cond_var.fun

  ### centralize X ###
  X = scale(X, center=TRUE, scale=F)

  ### Compute initial lasso estimator of beta ###
  if(is.null(beta.init)) beta.init = train.fun(X, y, lambda=lambda)$lasso.est
  beta.init = as.vector(beta.init)

  ### prepare values ###
  if(intercept) X = cbind(1, X)
  n = nrow(X); p = ncol(X)
  pred = as.vector(pred.fun(X%*%beta.init))
  deriv = as.vector(deriv.fun(X%*%beta.init))
  weight = as.vector(weight.fun(X%*%beta.init))
  cond_var = as.vector(cond_var.fun(pred, y))

  ### Adjust A and loading ###
  if(intercept) G = G+1
  if(nullA){
    A = t(X)%*%X / nrow(X)
    A = A[G,G,drop=F]
  }
  loading = rep(0, p)
  loading[G] = A%*%beta.init[G]
  loading.norm = sqrt(sum(loading^2))

  ################# Correction Direction ####################
  if(verbose) cat("Computing QF... \n")
  ### loading.norm too small, direction is set as 0 ###
  if(loading.norm <= 1e-5){
    cat("loading norm is too small, set projection direction as numeric(0) \n")
    direction = rep(0, length(loading))
  }
  ### loading.norm large enough ###
  if(loading.norm > 1e-5){
    if (n >= 6*p){
      temp = weight*deriv*X
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
            }else{
              Direction.Est = Direction_searchtuning(X, loading, weight=weight, deriv=deriv)
            }
          }
          ### cases when mu specified ###
          if(!is.null(mu)){
            Direction.Est = Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, mu=mu)
            while(Direction.Est$status!='optimal'){
              mu = mu*1.5
              Direction.Est = Direction_fixedtuning(X, loading, weight=weight, deriv=deriv, mu=mu)
            }
          }
          direction = Direction.Est$proj
          if(verbose) cat(paste0('The projection direction is identified at mu=', round(Direction.Est$mu, 6), '\n'))
        },
        error = function(e){
          message("Caught an error in CVXR during computing direction, switch to the alternative method")
          print(e)
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
  }

  ################# Bias Correction ################
  est.plugin = as.numeric(t(beta.init[G]) %*% A %*% beta.init[G])
  correction = 2 * mean((weight * (y-pred) * X) %*% direction)
  est.debias = as.numeric(est.plugin + correction * loading.norm)

  ############## Compute SE and Construct CI ###############
  V = 4 * sum(((sqrt(weight^2 * cond_var) * X) %*% direction)^2)/n * loading.norm^2
  if(nullA){
    V.add = sum((as.vector((X[,G,drop=F]%*%beta.init[G])^2) -
                   as.numeric(t(beta.init[G]) %*% A %*% beta.init[G]))^2) / n
  }else{
    V.add = 0
  }
  V.vec = (V + V.add + tau.vec)
  se.vec = sqrt(V.vec/n)
  ci.mat = cbind(est.debias - qnorm(1-alpha/2)*se.vec, est.debias + qnorm(1-alpha/2)*se.vec)
  colnames(ci.mat) = c("lower", "upper")
  rownames(ci.mat) = paste("tau",tau.vec,sep="")

  obj <- list(est.plugin = est.plugin,
              est.debias = est.debias,
              se.vec     = se.vec,
              ci.mat     = ci.mat,
              tau.vec    = tau.vec,
              proj       = direction * loading.norm)
  class(obj) = "QF"
  obj
}

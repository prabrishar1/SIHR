diagXtX <- function(x, MARGIN = 1, ...) {
  if(MARGIN == 1) {
    rowSums(x^2, ...)
  } else {
    rowSums(t(x)^2, ...)
  }
}

var.Sigma <- function(Z, gamma) {
  nsample <- dim(Z)[1] - 1
  v <- Z %*% gamma
  return(sum((diagXtX(v, MARGIN = 1) - sum(v^2) / nsample)^2) / nsample)
}

#' Inference for quadratic forms of the regression vector in high dimensional linear and logistic regressions
#'
#' @description Computes the bias-corrected estimator of the quadratic form of the regression vector, restricted to the set of indices \code{G} for the high dimensional linear and logistic regressions and the corresponding standard error.
#' It also constructs the confidence interval for the quadratic form and test whether it is above zero or not.             #removed
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param model The high dimensional regression model, either \code{linear} or \code{logistic}
#' @param G The set of indices, \code{G} in the quadratic form
#' @param Cov.weight Logical, if set to \code{TRUE} then \code{A } is the \eqn{|G|\times}\eqn{|G|} submatrix of the population covariance matrix corresponding to the index set \code{G}, else need to provide an \code{A} (default = TRUE)
#' @param A The matrix A in the quadratic form, of dimension \eqn{|G|\times}\eqn{|G|} (default = \code{NULL})
#' @param tau.vec The vector of enlargement factors for asymptotic variance of the bias-corrected estimator to handle super-efficiency (default = \eqn{1})
#' @param init.coef Initial estimator for the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of \code{init.coef} (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step The step size used to compute \code{mu}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis which claims that the quadratic form of the regression vector is equal to 0 (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the quadratic form of the regression vector}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The matrix of confidence interval for the quadratic form of the regression vector; row corresponds to different values of \code{tau.vec}}
#' \item{decision}{\code{decision}\eqn{=1} implies the quadratic form of the regression vector is above zero \eqn{\newline}
#' \code{decision}\eqn{=0} implies the quadratic form of the regression vector is zero \eqn{\newline}
#' row corresponds to different values of \code{tau.vec}}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in estimator for the quadratic form of the regression vector restricted to \code{G}}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm median na.omit
#' @importFrom scalreg scalreg
#' @import CVXR Matrix glmnet
#'
#' @examples
#' n = 100
#' p = 200
#' A1gen <- function(rho,p){
#' A1=matrix(0,p,p)
#' for(i in 1:p){
#'  for(j in 1:p){
#'    A1[i,j]<-rho^(abs(i-j))
#'  }
#' }
#' A1
#' }
#' mu <- rep(0,p)
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- c(1:10)/10
#' set.seed(1203)
#' X <- MASS::mvrnorm(n,mu,Cov)
#' exp_val <- X%*%beta
#' prob <- exp(exp_val)/(1+exp(exp_val))
#' set.seed(1203)
#' y <- rbinom(n,1,prob)
#' test.set =c(1:30)
#' Est <-SIHR::QF(X = X, y = y, model = "logistic", G = test.set, Cov.weight = FALSE, A = diag(length(test.set)))
#'
#' @references
#'
#' \insertRef{grouplin}{SIHR}
QF <- function(X, y, model = "linear", G, Cov.weight = TRUE, A = NULL, tau.vec = c(1), init.coef = NULL,
               lambda = NULL,  mu = NULL, step = NULL, resol = 1.5, maxiter = 6, alpha = 0.05, verbose = TRUE) {
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    stop("Error: Check dimensions of X and y")
  }
  else
  {
    if(Cov.weight == FALSE && is.null(A)){
      stop("Please provide matrix A")
    }else{
      data <- na.omit(data.frame(y,X))
      X <- as.matrix(data[,-1])
      y <- as.vector(data[,1])
      p <- ncol(X)
      n <- nrow(X)
      mean = colMeans(X)
      M = matrix(rep(mean,nrow(X)),byrow = T, nrow = nrow(X), ncol = ncol(X))
      X = X - M
      col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2))
      Xnor <- X %*% diag(col.norm)
      if(is.null(init.coef)){
        if(model == "logistic"){
          init.coef<- Initialization.step(X, y, model = "glm", lambda, intercept = TRUE)
        }else {
          init.coef<- Initialization.step(X, y, model, lambda, intercept = TRUE)
        }
        htheta <- init.coef$lasso.est[-1]
      } else {
        htheta <- init.coef
      }

      Xb <- Xnor
      Xc <- X
      pp <- p

      spar.est <- sum(abs(htheta) > 0.001)
      if(model == "linear"){
        sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / max(0.9*n, n - spar.est))
      }

      if (p == length(G)) {
        if(Cov.weight==TRUE)
        {
          lasso.plugin <- mean((Xc %*% htheta)^2)
        } else {
          lasso.plugin <- t(htheta)%*%A%*%htheta
        }
        direction <- htheta
        loading.norm <- 1
        test.vec <- htheta
      } else {
        sigma.hat <- (1 / (n-1)) * (t(Xc) %*% Xc)
        test.vec <- matrix(0, ncol = 1, nrow = pp)
        loading <- matrix(0, ncol = 1, nrow = pp)
        test.vec[G] <- htheta[G]

        if(Cov.weight==TRUE)
        {
          loading[G] <- (sigma.hat %*% test.vec)[G]
          lasso.plugin <- mean((Xc %*% test.vec)^2)
        } else {
          loading[G] <- (A %*% test.vec[G])
          lasso.plugin <- t(test.vec[G])%*%A%*%test.vec[G]
        }
        loading.norm <- sqrt(sum(loading^2))
        exp_pred = Xc%*%(htheta)
        if(model == "linear"){
          X.weight = Xc
        }else if(model == "logistic"){
          deriv.vec <- exp(exp_pred)/(1+exp(exp_pred))^2
          weight <- 1/deriv.vec
          X.weight = diag(c(sqrt(deriv.vec*weight))) %*% Xc
        }

        if (loading.norm == 0) {
          direction <- rep(0, pp)
        } else {
          if (n >= 6 * p) {
            tmp <- eigen(sigma.hat)
            tmp <- min(tmp$values) / max(tmp$values)
          } else {
            tmp <- 0
          }

          if ((n >= 6 * p) && (tmp >= 1e-4)) {
            direction <- solve(sigma.hat) %*% loading/loading.norm
          } else {
            if (is.null(step)) {
              step.vec <- rep(NA, 3)
              for (t in 1:3) {
                index.sel <- sample(1:n, size = ceiling(0.5 * min(n, p)), replace = FALSE)
                if(model=="linear"){
                  Direction.Est.temp <-  SIHR::Direction_searchtuning(Xc[index.sel, ],
                                                                      loading, model = "linear",
                                                                      resol = 1.5,maxiter = 6)
                } else if (model == "logistic"){
                  Direction.Est.temp <-  SIHR::Direction_searchtuning(Xc[index.sel, ],
                                                                      loading, model = "glm", weight = weight[index.sel], deriv.vec = deriv.vec[index.sel],
                                                                      resol = 1.5,maxiter = 6)
                }
                step.vec[t] <- Direction.Est.temp$step
              }
              step <-  getmode(step.vec)
              step
            }
            if(model == "linear"){
              Direction.Est <-  SIHR::Direction_fixedtuning(Xc, loading, model = "linear", mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
            } else if(model == "logistic"){
              Direction.Est <-  SIHR::Direction_fixedtuning(Xc, loading, model = "glm", mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)}, weight = weight, deriv.vec = deriv.vec)
            }

            while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
              step <- step-1
              if(model == "linear"){
                Direction.Est <-  SIHR::Direction_fixedtuning(Xc, loading, model = "linear", mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
              }else if(model == "logistic")
                Direction.Est <-  SIHR::Direction_fixedtuning(Xc, loading, model = "glm", mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)}, weight = weight, deriv.vec = deriv.vec)
            }
            if(verbose == TRUE){
              cat(paste("step is", step))
            }
            direction <- Direction.Est$proj
          }
        }
      }
      if(model == "linear"){
        weighed.residual = (y - exp_pred)
      } else if(model == "logistic"){
        weighed.residual <- (y - exp(exp_pred)/(1+ exp(exp_pred)))*weight
      }
      correction <- 2 * loading.norm * sum((Xc%*%direction)*weighed.residual)/n

      debias.est <- lasso.plugin + correction
      debias.est
      if(Cov.weight==TRUE)
      {
        se2 <- sqrt(var.Sigma(Xc, test.vec) / n)
        if(model == "linear"){
          se1 <- 2 * sd.est * sqrt(sum((Xc %*% direction)^2) / (n)^2) * loading.norm
        } else if (model == "logistic"){
          se1 <- 2 * sqrt(mean((Xc%*%direction)^2*weight^2*deriv.vec))*loading.norm/sqrt(n)
        }
        tau <- pmin(tau.vec, spar.est * log(p) / sqrt(n))
        se.vec <- sqrt(se1^2 + se2^2 + (tau / n))
      } else {
        if(model == "linear"){
          se <- 2*sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
        }else if (model == "logistic"){
          se <- 2 * sqrt(mean((Xc%*%direction)^2*weight^2*deriv.vec))*loading.norm/sqrt(n)
        }
        se.vec <- rep(NA,length(tau.vec))
        for (i in 1: length(tau.vec)){
          tau <- tau.vec[i]
          se <- sqrt(se^2+tau/n)
          se.vec[i] <- se
        }
      }
      #CI <- matrix(NA,nrow=length(tau.vec),ncol=2)
      #dec <- array(dim=1)
      #for(i in 1:length(tau.vec)){
      #  CI[i,] <- c(debias.est - qnorm(1-alpha/2)*se.vec[i], debias.est + qnorm(1-alpha/2)*se.vec[i])
      #  if(debias.est - qnorm(1-alpha)*se.vec[i] > 0){
      #    dec[i] <- 1
      #  } else {
      #    dec[i] <- 0
      #  }
      #}
      #returnList <- list("prop.est" = debias.est,
      #                   "se" = se.vec,
      #                   "CI" = CI,
      #                   "decision" = dec,
      #                   "proj"=direction,
      #                   "plug.in" = lasso.plugin)
      #return(returnList)
      out <- list(prop.est = debias.est,
                  se = se.vec,
                  proj = direction,
                  plug.in = lasso.plugin)
      structure(out, class = "QF")
    }
  }
}

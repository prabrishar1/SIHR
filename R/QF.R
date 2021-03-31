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

#' Inference for quadratic forms of the regression vector in high dimensional linear regression
#'
#' @description Computes the bias-corrected estimator of the quadratic form of the regression vector, restricted to the set of indices \code{G} for the high dimensional linear regression and the corresponding standard error.
#' It also constructs the confidence interval for the quadratic form and test whether it is above zero or not.
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param G The set of indices, \code{G} in the quadratic form
#' @param Cov.weight Logical, if set to \code{TRUE} then \code{A} is the population covariance matrix, else need to provide an \code{A} (default = TRUE)
#' @param A The matrix A in the quadratic form, of dimension \eqn{p\times}\eqn{p} (default = \eqn{I_{p \times p}})
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param tau.vec The vector of enlargement factors for asymptotic variance of the bias-corrected estimator to handle super-efficiency (default = \eqn{1})
#' @param init.Lasso Initial LASSO estimator for the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of initial LASSO estimator of the regression vector if \code{init.Lasso = NULL} (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step The step size used to compute \code{mu}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis which claims that the quadratic form of the regression vector is equal to 0 (default = 0.05)
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the quadratic form of the regression vector}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The matrix of confidence interval for the quadratic form of the regression vector; row corresponds to different values of \code{tau.vec}}
#' \item{decision}{\code{decision}\eqn{=1} implies the quadratic form of the regression vector is above zero \eqn{\newline}
#' \code{decision}\eqn{=0} implies the quadratic form of the regression vector is zero \eqn{\newline}
#' row corresponds to different values of \code{tau.vec}}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator for the quadratic form of the regression vector restricted to \code{G}}
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
#' mu[1:5] <- c(1:5)/5
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- c(1:10)/5
#' X <- MASS::mvrnorm(n,mu,Cov)
#' y = X%*%beta + rnorm(n)
#' test.set =c(30:50)
#' Est <-SIHR::QF(X = X, y = y, G = test.set)
#'
#' @references
#'
#' \insertRef{grouplin}{SIHR}
QF <- function(X, y, G, Cov.weight = TRUE, A = diag(ncol(X)), intercept = TRUE, tau.vec = c(1), init.Lasso = NULL,
               lambda = NULL,  mu = NULL, step = NULL, resol = 1.5, maxiter = 6, alpha = 0.05) {
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    print("Check dimensions of X and y")
  }
  else
  {
    data <- na.omit(data.frame(y,X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X)
    n <- nrow(X)
    col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2))
    Xnor <- X %*% diag(col.norm)
    if(is.null(init.Lasso)){
      init.Lasso<- Initialization.step(X,y,lambda,intercept)
      htheta <- init.Lasso$lasso.est
    } else {
      htheta <- init.Lasso
    }

    if (intercept == TRUE) {
      Xb <- cbind(rep(1,n),Xnor)
      Xc <- cbind(rep(1, n), X)
      pp <- (p + 1)
      G <- G + 1
    } else {
      Xb <- Xnor
      Xc <- X
      pp <- p
    }
    spar.est <- sum(abs(htheta) > 0.001)
    sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / max(0.9*n, n - spar.est))

    count=0
    for(i in 1:ncol(X)){
      if(length(unique(X[,i])) == 1){
        count = count + 1
      }
    }
    if(count!=0 && intercept==TRUE)
    {
      print("Data is singular")
    } else {
      if(Cov.weight == FALSE) {
        if(intercept==TRUE) {
          Ac <- rbind(c(1,rep(0,ncol(A))),cbind(rep(0,nrow(A)),A))
        } else {
          Ac <- A
        }
      }
      if (p == length(G)) {
        if(Cov.weight==TRUE)
        {
          lasso.plugin <- mean((Xc %*% htheta)^2)
        } else {
           print("Warning : Matrix A in the quadratic form is taken as the identity matrix, if not specified")
           lasso.plugin <- t(htheta)%*%Ac%*%htheta
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
          print("Warning : Matrix A in the quadratic form is taken as the identity matrix, if not specified")
          loading[G] <- (Ac %*% test.vec)[G]
          lasso.plugin <- t(test.vec)%*%Ac%*%test.vec
        }
        loading.norm <- sqrt(sum(loading^2))

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
            direction <- solve(sigma.hat) %*% loading
          } else {
            if (is.null(step)) {
              step.vec <- rep(NA, 3)
              for (t in 1:3) {
                index.sel <- sample(1:n, size = ceiling(0.5 * min(n, p)), replace = FALSE)
                Direction.Est.temp <-  Direction_searchtuning_lin(Xc[index.sel, ],
                                                                    loading, mu = NULL,
                                                                    resol = 1.5,maxiter = 6)
                step.vec[t] <- Direction.Est.temp$step
              }
              step <-  getmode(step.vec)
              step
            }
            Direction.Est <-  Direction_fixedtuning_lin(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
            while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
              step <- step-1
              Direction.Est <-  Direction_fixedtuning_lin(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
            }
            print(paste("step is", step))
            direction <- Direction.Est$proj
            sqrt(sum(direction^2))
          }
        }
      }
      correction <- 2 * loading.norm * t(Xc %*% direction) %*% (y - Xc %*% htheta) / n

      debias.est <- lasso.plugin + correction
      debias.est
      if(Cov.weight==TRUE)
      {
        se1 <- 2 * sd.est * sqrt(sum((Xc %*% direction)^2) / (n)^2) * loading.norm
        se2 <- sqrt(var.Sigma(Xc, test.vec) / n)
        tau <- pmin(tau.vec, spar.est * log(p) / sqrt(n))
        se.vec <- sqrt(se1^2 + se2^2 + (tau / n))
      } else {
        print("Warning : Matrix A in the quadratic form is taken as the identity matrix, if not specified")
        se <- 2*sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
        se.vec <- rep(NA,length(tau.vec))
        for (i in 1: length(tau.vec)){
          tau <- tau.vec[i]
          se <- sqrt(se^2+tau/n)
          se.vec[i] <- se
        }
      }
      CI <- matrix(NA,nrow=length(tau.vec),ncol=2)
      dec <- array(dim=1)
      for(i in 1:length(tau.vec)){
        CI[i,] <- c(debias.est - qnorm(1-alpha/2)*se.vec[i], debias.est + qnorm(1-alpha/2)*se.vec[i])
        if(debias.est - qnorm(1-alpha)*se.vec[i] > 0){
          dec[i] <- 1
          } else {
          dec[i] <- 0
          }
      }
      returnList <- list("prop.est" = debias.est,
                         "se" = se.vec,
                         "CI" = CI,
                         "decision" = dec,
                         "proj"=direction,
                         "plug.in" = lasso.plugin)
      return(returnList)
    }
  }
}

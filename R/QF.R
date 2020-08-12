###### Helper functions
# Calculate the mode
getmode <- function(v) {
  tbl <- table(v)
  if (all(tbl == 1)) {
    # case if all the values are distinct
    median(v)
  } else {
    # normal case if at least one value of v appears multiple times.
    as.numeric(names(which.max(tbl)))
  }

}

# A vectorized version to calculate the variance of each row or column.
diagXtX <- function(x, MARGIN = 1, ...) {
  if(MARGIN == 1) {
    # 1 indicates rows
    rowSums(x^2, ...)
  } else {
    # 2 indicates columns
    rowSums(t(x)^2, ...)
  }
}

var.Sigma <- function(Z, gamma) {
  nsample <- dim(Z)[1] - 1 # TODO why -1? B/c of intercept == TRUE or are two cases needed?
  v <- Z %*% gamma
  # return(sum((diag(v %*% t(v)) - sum(v^2) / nsample)^2) / nsample)
  return(sum((diagXtX(v, MARGIN = 1) - sum(v^2) / nsample)^2) / nsample)
}

Lasso <- function(X, y, lambda = NULL, intercept = TRUE) {

  p <- ncol(X)
  n <- nrow(X)

  htheta <- if (is.null(lambda)) {
    outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                        intercept = intercept)
    # Objective : 1/2 * RSS/n + lambda * penalty
    as.vector(coef(outLas, s = outLas$lambda.min))
  }
  else {
    outLas <- glmnet(X, y, family = "gaussian", alpha = 1,
                     intercept = intercept)
    # Objective : 1/2 * RSS/n + lambda * penalty
    as.vector(coef(outLas, s = lambda))
  }
  if (intercept == TRUE) {
    return(htheta)
  } else {
    return(htheta[2:(p+1)])
  }
}

###### The following function computes the lasso estimator
# Compute the Lasso estimator:
# - If lambda is given, use glmnet and standard Lasso
# - If lambda is not given, use square root Lasso
#Lasso <- function(X, y, lambda = NULL, intercept = TRUE) {
#  p <- ncol(X)
#  n <- nrow(X)

#  htheta <- if (is.null(lambda)) {
#   lambda <- sqrt(qnorm(1 - (0.1 / p)) / n)
#    outLas <- slim(X, y, lambda = lambda, method = "lq", q = 2,
#                   verbose = FALSE)
# Objective : sqrt(RSS/n) + lambda * penalty
#    c(as.vector(outLas$intercept), as.vector(outLas$beta))
#  } else {
#    outLas <- glmnet(X, y, family = "gaussian", alpha = 1,
#                     intercept = intercept)
# Objective : 1/2 * RSS/n + lambda * penalty
#    as.vector(coef(outLas, s = lambda))
#  }

#  if (intercept == TRUE) {
#    return(htheta)
#  } else {
#    return(htheta[2:(p+1)])
#  }
#}

###### The following function computes the inital Lasso estimator and
###### quantities based thereon
Initialization.step <- function(X, y, lambda = NULL, intercept = FALSE) {
  n <- nrow(X)
  # col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
  col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2))
  Xnor <- X %*% diag(col.norm)

  ### Call Lasso
  htheta <- Lasso(Xnor, y, lambda = lambda, intercept = intercept)

  ### Calculate return quantities
  if (intercept == TRUE) {
    Xb <- cbind(rep(1, n), Xnor)
    col.norm <- c(1, col.norm)
  } else {
    Xb <- Xnor
  }
  sparsity <- sum(abs(htheta) > 0.001)
  sd.est <- sum((y - Xb %*% htheta)^2) / n
  htheta <- htheta * col.norm
  returnList <- list("lasso.est" = htheta,
                     "sigma" = sd.est,
                     "sparsity" = sparsity)
  return(returnList)
}
###### Direction_fixedtuning_robust is searching for the projection direction
###### for a general class of loadings including dense loadings
Direction_fixedtuning_robust <- function(Xc, loading, mu = NULL) {
  pp <- ncol(Xc)
  n <- nrow(Xc)
  if (is.null(mu)) {
    mu <- sqrt(2.01 * log(pp) / n)
  }
  loading.norm <- sqrt(sum(loading^2))
  H <- cbind(loading / loading.norm, diag(1, pp))

  # Optimization
  v <- Variable(pp + 1)
  obj <- 1/4 * sum((Xc %*% H %*% v)^2) / n +
    sum((loading / loading.norm) * (H %*% v)) + mu * sum(abs(v))
  prob <- Problem(Minimize(obj))
  result <- solve(prob, solver = "SCS")

  # print("fixed mu")
  # print(mu)
  # print(result$value)
  opt.sol <- result$getValue(v)
  # cvxr_status <- result$status

  # Return the projection direction
  direction <- (-1) / 2 * (opt.sol[-1] + opt.sol[1] * loading / loading.norm)
  returnList <- list("proj" = direction)
  return(returnList)
}

###### Direction_searchtuning_robust is combining Direction_fixedtuning_robust
###### with parameter searching
Direction_searchtuning_robust <- function(Xc, loading, mu = NULL, resol = 1.5,
                                          maxiter = 10) {
  pp <- ncol(Xc)
  n <- nrow(Xc)
  tryno <- 1
  opt.sol <- rep(0, pp)
  lamstop <- 0
  cvxr_status <- "optimal"
  mu <- sqrt(2.01 * log(pp) / n)

  ### This iteration finds a good tuning parameter
  while (lamstop == 0 && tryno < maxiter) {
    # print(mu)
    lastv <- opt.sol
    lastresp <- cvxr_status
    loading.norm <- sqrt(sum(loading^2))
    H <- cbind(loading / loading.norm, diag(1, pp))

    # Optimization
    v <- Variable(pp + 1)
    obj <- 1 / 4 * sum((Xc %*% H %*% v)^2) / n +
      sum((loading / loading.norm) * (H %*% v)) + mu * sum(abs(v))
    prob <- Problem(Minimize(obj))
    result <- solve(prob, solver = "SCS")
    opt.sol <- result$getValue(v)
    cvxr_status <- result$status

    if (tryno == 1) {
      if (cvxr_status == "optimal") {
        incr <- 0
        mu <- mu / resol
        temp.vec <- (-1) / 2 * (opt.sol[-1] + opt.sol[1] * loading / loading.norm)
        initial.sd <- sqrt(sum((Xc %*% temp.vec)^2) / (n)^2) * loading.norm
        temp.sd <- initial.sd
      } else {
        incr <- 1
        mu <- mu * resol
      }
    } else {
      if (incr == 1) { # if the tuning parameter is increased in the last step
        if (cvxr_status == "optimal") {
          lamstop <- 1
        } else {
          mu <- mu * resol
        }
      } else {
        if (cvxr_status == "optimal" && temp.sd < 3 * initial.sd) {
          mu <- mu / resol
          temp.vec <- (-1) / 2 * (opt.sol[-1] + opt.sol[1] * loading / loading.norm)
          temp.sd <- sqrt(sum((Xc %*% temp.vec)^2) / (n)^2) * loading.norm
        } else {
          mu <- mu * resol
          opt.sol <- lastv
          lamstop <- 1
          tryno <- tryno - 1
        }
      }
    }
    tryno  <- tryno + 1
  }

  # Return the projection direction
  direction <- (-1) / 2 * (opt.sol[-1] + opt.sol[1] * loading / loading.norm)
  step <- tryno - 1
  # print(step)
  returnList <- list("proj" = direction,
                     "step" = step)
  return(returnList)
}

###### The following function calculates the debiased point estimate for the
###### group of interest
# If the test.set is the vector 1:p, then the solution of global test
# can be calculated explicitly.

#' Inference for quadratic functional in high-dimensional linear regression model
#'
#' @description Computes the bias-corrected estimator of the quadratic functional restricted to group \code{G} for the high-dimensional linear regression model and the corresponding standard error
#'
#' @param X Design matrix
#' @param y Response variable
#' @param test.set set of indices, \code{G} in the quadratic functional
#' @param A Matrix A in the quadratic functional (either the population covariance matrix \code{sigma} or a known matrix of suitable dimension ; default = \code{sigma})
#' @param init.Lasso initial LASSO estimator for the regression vector (default = \code{NULL})
#' @param tau.vec Vector of enlargement factors for asymptotic variance of the bias-corrected estimator to handle super-efficiency (default = \code{NULL})
#' @param lambda Tuning parameter used in construction of initial LASSO estimator of the regression vector if \code{init.Lasso = NULL} (default = \code{NULL})
#' @param intercept Should intercept(s) be fitted (default = \code{FALSE})
#' @param mu Tuning parameter in construction of the projection direction (default = \code{NULL})
#' @param step Number of steps (< \code{maxiter}) to obtain the smallest \code{mu} that gives convergence of the
#' optimization problem for constructing the projection direction (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction (default = 10)
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the quadratic functional restricted to \code{G}}
#' \item{sigma}{Estimate of the error variance in the linear regression model}
#' \item{se}{Standard error of the bias-corrected estimator}
#' \item{plug.in}{Plug-in LASSO estimator for the quadratic functional restricted to \code{G}}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm
#' @import CVXR Matrix glmnet
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' beta = (1:400)/25
#' y = X%*%beta + rnorm(100,0,1)
#' QF(X = X, y = y, test.set=c(30:50))
#'
#' @references
#'
#' \insertRef{grouplin}{FIHR}
QF <- function(X, y, test.set, A = "sigma",init.Lasso = NULL, tau.vec = NULL,
                           lambda = NULL, intercept = FALSE, mu = NULL,
                           step = NULL, resol = 1.5, maxiter = 6) {

  if (is.null(init.Lasso)) {
    ### Inital Lasso estimate of beta and sigma
    init.Lasso <- Initialization.step(X, y, lambda, intercept)
  }
  htheta <- init.Lasso$lasso.est # beta
  sd.est <- init.Lasso$sigma # sigma
  spar.est <- init.Lasso$sparsity

  p <- ncol(X)
  n <- nrow(X)
  if (intercept == TRUE) {
    Xc <- cbind(rep(1, n), X)
    pp <- (p + 1)
    test.set <- test.set + 1
  } else {
    Xc <- X
    pp <- p
  }

  ## search for projection direction
  if (p == length(test.set)) {
    ## Global Test
    ## We do not need not search for a direction, i.e. hat{u} = hat{theta}.
    if(A=="sigma")
    {
      lasso.plugin <- mean((Xc %*% htheta)^2)
    }
    else
    {
      lasso.plugin <- t(htheta)%*%A%*%htheta
    }
    direction <- htheta
    loading.norm <- 1
    test.vec <- htheta
  } else {

    ## Prepare all quantities
    sigma.hat <- (1 / (n-1)) * (t(Xc) %*% Xc)
    test.vec <- matrix(0, ncol = 1, nrow = pp)
    loading <- matrix(0, ncol = 1, nrow = pp)
    test.vec[test.set] <- htheta[test.set]

    if(A=="sigma")
    {
      loading[test.set] <- (sigma.hat %*% test.vec)[test.set]
      lasso.plugin <- mean((Xc %*% test.vec)^2)
    }
    else
    {
      loading[test.set] <- (A %*% test.vec)[test.set]
      lasso.plugin <- t(test.vec)%*%A%*%test.vec
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

      ## Search for projection direction u
      if ((n >= 6 * p) && (tmp >= 1e-4)) {
        # sigma.hat matrix is well conditioned
        direction <- solve(sigma.hat) %*% loading
      } else {
        if (n > 0.5 * p) {
          # for option 1
          if (is.null(step)) {
            step.vec <- rep(NA, 3)
            for (t in 1:3) {
              index.sel <- sample(1:n, size = ceiling(0.5 * min(n, p)), replace = FALSE)
              Direction.Est.temp <- Direction_searchtuning_robust(Xc[index.sel, ],
                                                                  loading, mu = NULL,
                                                                  resol = 1.5,
                                                                  maxiter = 6)
              step.vec[t] <- Direction.Est.temp$step
            }
            step <- getmode(step.vec)
          }
          Direction.Est <- Direction_fixedtuning_robust(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
        } else {
          # for option 2
          Direction.Est <- Direction_searchtuning_robust(Xc, loading, mu = NULL, resol, maxiter)
          step <- Direction.Est$step
        }
        # print(paste("step is", step))
        direction <- Direction.Est$proj
      }
    }

  }

  ### Correct the initial estimator by the constructed projection direction
  correction <- 2 * loading.norm * t(Xc %*% direction) %*% (y - Xc %*% htheta) / n
  debias.est <- lasso.plugin + correction
  if(A=="sigma")
  {
    se1 <- 2 * sd.est * sqrt(sum((Xc %*% direction)^2) / (n)^2) * loading.norm
    se2 <- sqrt(var.Sigma(Xc, test.vec) / n)

    if (is.null(tau.vec)) { # TODO maybe just set as default argument in function call.
      tau.vec <- 1
    }

    if (abs(correction) > abs(lasso.plugin)) {
      warning(paste("The model is most likely misspecified because the correction term is larger than the lasso estimate in absolute value.",
                    "See cluster or group: ", paste(colnames(Xc)[test.set], collapse = ", "),
                    ". The value of the lasso.plugin and correction are", round(lasso.plugin, 5),
                    " respectively ", round(correction, 5), "."))
    }

    # TODO Is tau.vec a vector in the future? Or change pmin to min.
    ### Correct standard error value by tau.
    tau <- pmin(tau.vec, spar.est * log(p) / sqrt(n))
    se.vec <- sqrt(se1^2 + se2^2 + (tau / n))
  }
  else
  {
    se<-2*sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
    #tau=0
    if(is.null(tau.vec)){
      tau.vec=c(2)
    }
    se.vec<-rep(NA,length(tau.vec))
    for (i in 1: length(tau.vec)){
      tau=min(tau.vec[i],spar.est*log(p)/sqrt(n))
      se<-sqrt(se^2+tau/n)
      se.vec[i]<-se
    }
  }


  returnList <- list("prop.est" = debias.est,
                     "sigma"= sd.est,
                     "se" = se.vec,
                     "plug.in" = lasso.plugin)
  return(returnList)
}


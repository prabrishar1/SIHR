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
  nsample <- dim(Z)[1] - 1
  v <- Z %*% gamma
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
  } else if (lambda == "CV") {
    outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                        intercept = intercept)
    # Objective : 1/2 * RSS/n + lambda * penalty
    as.vector(coef(outLas, s = outLas$lambda.1se))
  } else if (lambda == "scalreg") {
    Xc <- if (intercept) {
      cbind(rep(1, n), X)
    } else {
      X
    }
    outLas <- scalreg(Xc, y)
    # return object
    if (intercept) {
      outLas$coefficients
    } else {
      # add a coefficient for the (not estimated) intercept b/c of implementation
      c(0, outLas$coefficients)
    }
  } else {
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

Initialization.step <- function(X, y, lambda = NULL, intercept = FALSE) {
  n <- nrow(X)
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
  htheta <- htheta * col.norm
  returnList <- list("lasso.est" = htheta)
  return(returnList)
}

Direction_fixedtuning_robust<-function(X,loading,mu=NULL){
  pp<-ncol(X)
  n<-nrow(X)
  if(is.null(mu)){
    mu<-sqrt(2.01*log(pp)/n)
  }
  loading.norm<-sqrt(sum(loading^2))

  if (loading.norm==0){
    H <- cbind(loading, diag(1, pp))
  }else{
    H <- cbind(loading / loading.norm, diag(1, pp))
  }

  v<-Variable(pp+1)
  obj<-1/4*sum((X%*%H%*%v)^2)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
  prob<-Problem(Minimize(obj))
  result<-solve(prob)
  print("fixed mu")
  print(mu)
  opt.sol<-result$getValue(v)
  cvxr_status<-result$status
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  returnList <- list("proj"=direction)
  return(returnList)
}

Direction_searchtuning_robust<-function(X,loading,mu=NULL, resol = 1.5, maxiter = 10){
  pp<-ncol(X)
  n<-nrow(X)
  tryno = 1;
  opt.sol = rep(0,pp+1);
  lamstop = 0;
  cvxr_status = "optimal";

  mu = sqrt(2.01*log(pp)/n);
  while (lamstop == 0 && tryno < maxiter){
    ###### This iteration is to find a good tuning parameter
    lastv = opt.sol;
    lastresp = cvxr_status;
    loading.norm<-sqrt(sum(loading^2))

    if (loading.norm==0){
      H <- cbind(loading, diag(1, pp))
    }else{
      H <- cbind(loading / loading.norm, diag(1, pp))
    }

    v<-Variable(pp+1)
    obj<-1/4*sum((X%*%H%*%v)^2)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
    prob<-Problem(Minimize(obj))
    result<-solve(prob)
    cvxr_status<-result$status
    if(tryno==1){
      if(cvxr_status=="optimal"){
        incr = 0;
        mu=mu/resol;

        opt.sol<-result$getValue(v)

        temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
        initial.sd<-sqrt(sum((X%*% temp.vec)^2)/(n)^2)*loading.norm
        temp.sd<-initial.sd
      }else{
        incr = 1;
        mu=mu*resol;
      }
    }else{
      if(incr == 1){ ### if the tuning parameter is increased in the last step
        if(cvxr_status=="optimal"){

          opt.sol<-result$getValue(v)

          lamstop = 1;
        }else{
          mu=mu*resol;
        }
      }else{
        if(cvxr_status=="optimal"&&temp.sd<3*initial.sd){
          mu = mu/resol;

          opt.sol <- result$getValue(v)

          temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
          temp.sd<-sqrt(sum((X%*% temp.vec)^2)/(n)^2)*loading.norm
        }else{
          mu=mu*resol;
          opt.sol=lastv;
          lamstop=1;
          tryno=tryno-1
        }
      }
    }
    tryno = tryno + 1;
  }
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  step<-tryno-1
  print(step)
  returnList <- list("proj"=direction,
                     "step"=step)
  return(returnList)
}

#' Inference for quadratic functional in high dimensional linear regression
#'
#' @description Computes the bias-corrected estimator of the quadratic functional \eqn{\beta_G^{\top}A\beta_G} for the high dimensional linear regression \eqn{Y_i = X_i^{\top}\beta + \epsilon} and the corresponding standard error.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param G The set of indices, \code{G} in \eqn{\beta_G^{\top}A\beta_G}
#' @param Cov.weight Logical, if set to \code{TRUE} then \code{A}\eqn{=\Sigma}, else need to provide an \code{A} (default = TRUE)
#' @param A The matrix A in the quadratic functional, of dimension \eqn{p}x\eqn{p}, when \code{Cov.weight = FALSE}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param init.Lasso Initial LASSO estimator for the regression vector (default = \code{NULL})
#' @param tau.vec The vector of enlargement factors for asymptotic variance of the bias-corrected estimator to handle super-efficiency (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of initial LASSO estimator of the regression vector if \code{init.Lasso = NULL} (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step Number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 10)
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator of the quadratic functional restricted to \code{G}}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator for the quadratic functional restricted to \code{G}}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm median na.omit
#' @importFrom scalreg scalreg
#' @import CVXR Matrix glmnet
#'
#' @examples
#' n = 100
#' p = 400
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
#' Est <-FIHR::QF(X = X, y = y, G = test.set)
#'
#' @references
#'
#' \insertRef{grouplin}{FIHR}
QF <- function(X, y, G, Cov.weight = TRUE, A = NULL,init.Lasso = NULL, tau.vec = NULL,
               lambda = NULL, intercept = TRUE, mu = NULL,
               step = NULL, resol = 1.25, maxiter = 10) {
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    print("Check dimensions of X and y")
  }
  else
  {
    data = na.omit(data.frame(y,X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X);
    n <- nrow(X);
    col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2));
    Xnor <- X %*% diag(col.norm);
    if(is.null(init.Lasso)){
      ####### implement a lasso algorithm to get beta and sigma
      init.Lasso<-Initialization.step(X,y,lambda,intercept)
      htheta<-init.Lasso$lasso.est
    }
    else
    {
      htheta<- init.Lasso
    }

    if (intercept == TRUE) {
      Xb <- cbind(rep(1,n),Xnor);
      Xc <- cbind(rep(1, n), X)
      pp <- (p + 1)
      G <- G + 1
    } else {
      Xb <- Xnor;
      Xc <- X
      pp <- p
    }

    #htheta<-init.Lasso$lasso.est
    spar.est <- sum(abs(htheta) > 0.001)
    sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / max(0.9*n, n - spar.est))

    count=0
    for(i in 1:ncol(X)){
      if(length(unique(X[,i]))==1){
        count=count+1
      }
    }
    if(count!=0 && intercept==TRUE)
    {
      print("Data is singular")
    }else{
      if(Cov.weight==FALSE)
      {
        if(intercept==TRUE)
        {
          Ac <- rbind(c(1,rep(0,ncol(A))),cbind(rep(0,nrow(A)),A))
        }
        else
        {
          Ac <- A
        }
      }

      ## search for projection direction
      if (p == length(G)) {
        ## Global Test
        ## We do not need not search for a direction, i.e. hat{u} = hat{theta}.
        if(Cov.weight==TRUE)
        {
          lasso.plugin <- mean((Xc %*% htheta)^2)
        }
        else
        {
          if(is.null(A))
          {
            print("Need to provide a known square matrix A of dimension p, results are faulty")
          }
          else{
            lasso.plugin <- t(htheta)%*%Ac%*%htheta
          }
        }
        direction <- htheta
        loading.norm <- 1
        test.vec <- htheta
      } else {

        ## Prepare all quantities
        sigma.hat <- (1 / (n-1)) * (t(Xc) %*% Xc)
        test.vec <- matrix(0, ncol = 1, nrow = pp)
        loading <- matrix(0, ncol = 1, nrow = pp)
        test.vec[G] <- htheta[G]

        if(Cov.weight==TRUE)
        {
          loading[G] <- (sigma.hat %*% test.vec)[G]
          lasso.plugin <- mean((Xc %*% test.vec)^2)
        }
        else
        {
          if(is.null(A))
          {
            print("Need to provide a known square matrix A of dimension p, results are faulty")
          }
          else{
            loading[G] <- (Ac %*% test.vec)[G]
            lasso.plugin <- t(test.vec)%*%Ac%*%test.vec
          }
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
            if (is.null(step)) {
              step.vec <- rep(NA, 3)
              for (t in 1:3) {
                index.sel <- sample(1:n, size = ceiling(0.5 * min(n, p)), replace = FALSE)
                Direction.Est.temp <- Direction_searchtuning_robust(Xc[index.sel, ],
                                                                    loading, mu = NULL,
                                                                    resol = 1.25,maxiter = 6)
                step.vec[t] <- Direction.Est.temp$step
              }
              step <- getmode(step.vec)
              step
            }
            Direction.Est <- Direction_fixedtuning_robust(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
            while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
              step<-step-1
              Direction.Est <- Direction_fixedtuning_robust(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
            }
            print(paste("step is", step))
            direction <- Direction.Est$proj
            sqrt(sum(direction^2))
          }
        }

      }

      ### Correct the initial estimator by the constructed projection direction
      correction <- 2 * loading.norm * t(Xc %*% direction) %*% (y - Xc %*% htheta) / n

      debias.est <- lasso.plugin + correction
      debias.est
      if(Cov.weight==TRUE)
      {
        se1 <- 2 * sd.est * sqrt(sum((Xc %*% direction)^2) / (n)^2) * loading.norm
        se2 <- sqrt(var.Sigma(Xc, test.vec) / n)

        if (is.null(tau.vec)) {
          tau.vec <- 1
        }

        if (abs(correction) > abs(lasso.plugin)) {
          warning(paste("The model is most likely misspecified because the correction term is larger than the lasso estimate in absolute value.",
                        "See cluster or group: ", paste(colnames(Xc)[G], collapse = ", "),
                        ". The value of the lasso.plugin and correction are", round(lasso.plugin, 5),
                        " respectively ", round(correction, 5), "."))
        }

        ### Correct standard error value by tau.
        tau <- pmin(tau.vec, spar.est * log(p) / sqrt(n))
        se.vec <- sqrt(se1^2 + se2^2 + (tau / n))
      }
      else
      {
        if(is.null(A))
        {
          print("Need to provide a known square matrix A of dimension p, results are faulty")
        }
        else{
          se<-2*sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
          if(is.null(tau.vec)){
            tau.vec=c(0.5)
          }
          se.vec<-rep(NA,length(tau.vec))
          for (i in 1: length(tau.vec)){
            tau = tau.vec[i]
            se<-sqrt(se^2+tau/n)
            se.vec[i]<-se
          }
        }
      }
      returnList <- list("prop.est" = debias.est,
                         "se" = se.vec,
                         "proj"=direction,
                         "plug.in" = lasso.plugin)
      return(returnList)
    }
  }
}

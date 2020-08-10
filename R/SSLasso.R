### read the package and the source code
###source('~/Dropbox/Projects and Papers/Treatment Selection/Code/lasso_inference.r')
#library(MASS)
#library(AER)
#library(Matrix);
#library(glmnet);
#library(flare);



SoftThreshold <- function( x, lambda ) {
  #
  # Standard soft thresholding
  #
  if (x>lambda){
    return (x-lambda);}
  else {
    if (x< (-lambda)){
      return (x+lambda);}
    else {
      return (0); }
  }
}

InverseLinftyOneRow <- function ( sigma, i, mu, maxiter=50, threshold=1e-2 ) {
  p <- nrow(sigma);
  rho <- max(abs(sigma[i,-i])) / sigma[i,i];
  mu0 <- rho/(1+rho);
  beta <- rep(0,p);

  if (mu >= mu0){
    beta[i] <- (1-mu0)/sigma[i,i];
    returnlist <- list("optsol" = beta, "iter" = 0);
    return(returnlist);
  }

  diff.norm2 <- 1;
  last.norm2 <- 1;
  iter <- 1;
  iter.old <- 1;
  beta[i] <- (1-mu0)/sigma[i,i];
  beta.old <- beta;
  sigma.tilde <- sigma;
  diag(sigma.tilde) <- 0;
  vs <- -sigma.tilde%*%beta;

  while ((iter <= maxiter) && (diff.norm2 >= threshold*last.norm2)){

    for (j in 1:p){
      oldval <- beta[j];
      v <- vs[j];
      if (j==i)
        v <- v+1;
      beta[j] <- SoftThreshold(v,mu)/sigma[j,j];
      if (oldval != beta[j]){
        vs <- vs + (oldval-beta[j])*sigma.tilde[,j];
      }
    }

    iter <- iter + 1;
    if (iter==2*iter.old){
      d <- beta - beta.old;
      diff.norm2 <- sqrt(sum(d*d));
      last.norm2 <-sqrt(sum(beta*beta));
      iter.old <- iter;
      beta.old <- beta;
      if (iter>10)
        vs <- -sigma.tilde%*%beta;
    }
  }

  returnlist <- list("optsol" = beta, "iter" = iter)
  return(returnlist)
}

InverseLinfty <- function(sigma, n, resol=1.5, mu=NULL, maxiter=50, threshold=1e-2, verbose = TRUE) {
  isgiven <- 1;
  if (is.null(mu)){
    isgiven <- 0;
  }

  p <- nrow(sigma);
  M <- matrix(0, p, p);
  xperc = 0;
  xp = round(p/10);
  for (i in 1:p) {
    if ((i %% xp)==0){
      xperc = xperc+10;
      if (verbose) {
        print(paste(xperc,"% done",sep="")); }
    }
    if (isgiven==0){
      mu <- (1/sqrt(n)) * qnorm(1-(0.1/(p^2)));
    }
    mu.stop <- 0;
    try.no <- 1;
    incr <- 0;
    while ((mu.stop != 1)&&(try.no<10)){
      last.beta <- beta
      output <- InverseLinftyOneRow(sigma, i, mu, maxiter=maxiter, threshold=threshold)
      beta <- output$optsol
      iter <- output$iter
      if (isgiven==1){
        mu.stop <- 1
      }
      else{
        if (try.no==1){
          if (iter == (maxiter+1)){
            incr <- 1;
            mu <- mu*resol;
          } else {
            incr <- 0;
            mu <- mu/resol;
          }
        }
        if (try.no > 1){
          if ((incr == 1)&&(iter == (maxiter+1))){
            mu <- mu*resol;
          }
          if ((incr == 1)&&(iter < (maxiter+1))){
            mu.stop <- 1;
          }
          if ((incr == 0)&&(iter < (maxiter+1))){
            mu <- mu/resol;
          }
          if ((incr == 0)&&(iter == (maxiter+1))){
            mu <- mu*resol;
            beta <- last.beta;
            mu.stop <- 1;
          }
        }
      }
      try.no <- try.no+1
    }
    M[i,] <- beta;
  }
  return(M)
}

NoiseSd <- function( yh, A, n ){
  ynorm <- sqrt(n)*(yh/sqrt(diag(A)));
  sd.hat0 <- mad(ynorm);

  zeros <- (abs(ynorm)<3*sd.hat0);
  y2norm <- sum(yh[zeros]^2);
  Atrace <- sum(diag(A)[zeros]);
  sd.hat1 <- sqrt(n*y2norm/Atrace);

  ratio <- sd.hat0/sd.hat1;
  if (max(ratio,1/ratio)>2)
    print("Warning: Noise estimate problematic");

  s0 <- sum(zeros==FALSE);
  return (list( "sd" = sd.hat1, "nz" = s0));
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

#' Compute confidence intervals and p-values.
#'
#' @description
#' Compute confidence intervals and p-values.
#'
#' @param X Design matrix, of dimensions nobs(n) x nvar(p)
#' @param y Response vector
#' @param alpha Significance level
#' @param lambda Lasso regularization parameter (if \code{NULL}, fixed by square-root LASSO)
#' @param mu \eqn{\ell_{\infty}} constraint on M (if \code{NULL}, searches)
#' @param intercept Should intercept(s) be fitted (default = \code{FALSE})
#' @param resol Step parameter for the function that computes M
#' @param maxiter Iteration parameter for computing M
#' @param threshold Tolerance criterion for computing M
#' @param verbose verbose
#'
#' @return
#' \item{noise.sd}{Estimate of the noise standard deviation}
#' \item{norm0}{Estimate of the number of 'significant' coefficients}
#' \item{coef}{Lasso estimated coefficients}
#' \item{unb.coef}{Unbiased coefficient estimates}
#' \item{low.lim}{Lower limits of confidence intervals}
#' \item{up.lim}{upper limit of confidence intervals}
#' \item{pvals}{p-values for the coefficients}
#' @export
#'
#' @importFrom stats coef qnorm
#' @import CVXR AER Matrix glmnet
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' beta = (1:400)/25
#' y = X%*%beta + rnorm(100,0,1)
#' SSLasso(X = X, y = y)
SSLasso <- function (X, y, alpha=0.05, lambda = NULL, mu = NULL, intercept = TRUE,
                     resol=1.3, maxiter=50, threshold=1e-2, verbose = TRUE) {
  #
  # Compute confidence intervals and p-values.
  #
  # Args:
  #   X     :  design matrix
  #   y     :  response
  #   alpha :  significance level
  #   lambda:  Lasso regularization parameter (if null, fixed by sqrt lasso)
  #   mu    :  Linfty constraint on M (if null, searches)
  #   resol :  step parameter for the function that computes M
  #   maxiter: iteration parameter for computing M
  #   threshold : tolerance criterion for computing M
  #   verbose : verbose?
  #
  # Returns:
  #   noise.sd: Estimate of the noise standard deviation
  #   norm0   : Estimate of the number of 'significant' coefficients
  #   coef    : Lasso estimated coefficients
  #   unb.coef: Unbiased coefficient estimates
  #   low.lim : Lower limits of confidence intervals
  #   up.lim  : upper limit of confidence intervals
  #   pvals   : p-values for the coefficients
  #
  p <- ncol(X);
  n <- nrow(X);
  pp <- p;
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  X <- X %*% diag(col.norm);

  htheta <- Lasso (X,y,lambda=lambda,intercept=intercept);

  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
  } else {
    Xb <- X;
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);

  if ((n>=2*p)){
    tmp <- eigen(sigma.hat)
    tmp <- min(tmp$values)/max(tmp$values)
  }else{
    tmp <- 0
  }

  if ((n>=2*p)&&(tmp>=1e-4)){
    M <- solve(sigma.hat)
  }else{
    M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
  }

  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n);

  A <- M %*% sigma.hat %*% t(M);
  projection<-M%*%t(Xb)
  noise <- NoiseSd( unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  Cov <- s.hat^2*A/n

  #sd.vector <- s.hat*sqrt(diag(A))/(sqrt(n));
  sd.vector <- sqrt(diag(Cov));
  #if  (is.null(lambda)){
  #  lambda <- s.hat*sqrt(qnorm(1-(0.1/p))/n);
  #}

  #addlength <- rep(0,pp);
  #MM <- M%*%sigma.hat - diag(pp);
  #for (i in 1:pp){
  #	effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
  #	effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
  #	addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  #}

  htheta <- htheta*col.norm;
  unbiased.Lasso <- unbiased.Lasso*col.norm;
  sd.vector <- sd.vector*col.norm;
  projection<-diag(col.norm)%*%projection
  #addlength <- addlength*col.norm;

  #if (intercept==TRUE){
  #  unbiased.Lasso <- unbiased.Lasso[2:pp];
  #  sd.vector <- sd.vector[2:pp];
  #}
  #p.vals <- 2*(1-pnorm(sqrt(n)*abs(unbiased.Lasso)/(s.hat*col.norm[(pp-p+1):pp]*sqrt(diag(A[(pp-p+1):pp,(pp-p+1):pp])))))

  returnList <- list("noise.sd" = s.hat,
                     "norm0" = noise$nz,
                     "coef" = htheta,
                     "unb.coef" = unbiased.Lasso,
                     "size" = sd.vector,
                     "proj"=projection,
                     "Cov"=Cov
  )
  return(returnList)
}

SSLasso.known <- function (X, y, design.cov=NULL,alpha=0.05, lambda = NULL, mu = NULL, intercept = TRUE,resol=1.3, maxiter=50, threshold=1e-2, verbose = TRUE) {
  #
  # Compute confidence intervals and p-values.
  #
  # Args:
  #   X     :  design matrix
  #   y     :  response
  #   design.cov: covariance matrix of the design X
  #   alpha :  significance level
  #   lambda:  Lasso regularization parameter (if null, fixed by sqrt lasso)
  #   mu    :  Linfty constraint on M (if null, searches)
  #   resol :  step parameter for the function that computes M
  #   maxiter: iteration parameter for computing M
  #   threshold : tolerance criterion for computing M
  #   verbose : verbose?
  #
  # Returns:
  #   noise.sd: Estimate of the noise standard deviation
  #   norm0   : Estimate of the number of 'significant' coefficients
  #   coef    : Lasso estimated coefficients
  #   unb.coef: Unbiased coefficient estimates
  #   low.lim : Lower limits of confidence intervals
  #   up.lim  : upper limit of confidence intervals
  #   pvals   : p-values for the coefficients
  #
  p <- ncol(X);
  n <- nrow(X);
  pp <- p;
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  X <- X %*% diag(col.norm);

  htheta <- Lasso (X,y,lambda=lambda,intercept=intercept);

  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
    sigma.known <- diag(col.norm)%*%design.cov%*%diag(col.norm)
  } else {
    Xb <- X;
    sigma.known <- diag(col.norm)%*%design.cov%*%diag(col.norm)
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);

  # if ((n>=2*p)){
  # tmp <- eigen(sigma.hat)
  # tmp <- min(tmp$values)/max(tmp$values)
  # }else{
  # tmp <- 0
  # }

  # if ((n>=2*p)&&(tmp>=1e-4)){
  #
  # }else{
  # M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
  # }
  M <- solve(sigma.known)
  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n);

  A <- M %*% sigma.hat %*% t(M);
  projection<-M%*%t(Xb)
  noise <- NoiseSd( unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  Cov <- s.hat^2*A/n

  #sd.vector <- s.hat*sqrt(diag(A))/(sqrt(n));
  sd.vector <- sqrt(diag(A));
  #if  (is.null(lambda)){
  #  lambda <- s.hat*sqrt(qnorm(1-(0.1/p))/n);
  #}

  #addlength <- rep(0,pp);
  #MM <- M%*%sigma.hat - diag(pp);
  #for (i in 1:pp){
  #	effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
  #	effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
  #	addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  #}

  htheta <- htheta*col.norm;
  Cov <- diag(col.norm)%*%Cov%*%diag(col.norm);
  unbiased.Lasso <- unbiased.Lasso*col.norm;
  sd.vector <- sd.vector*col.norm;
  projection<-diag(col.norm)%*%projection

  #if (intercept==TRUE){
  #  unbiased.Lasso <- unbiased.Lasso[2:pp];
  #  sd.vector <- sd.vector[2:pp];
  #}
  returnList <- list("unb.coef" = unbiased.Lasso,
                     "Cov"=Cov,
                     "noise.sd" = s.hat,
                     "norm0" = noise$nz,
                     "coef" = htheta,
                     "size" = sd.vector,
                     "proj"=projection
  )
  return(returnList)
}


SoftThreshold <- function( x, lambda ) {
  #
  # Standard soft thresholding
  #
  if (x>lambda){
    return (x-lambda);}
  else {
    if (x< (-lambda)){
      return (x+lambda);}
    else {
      return (0); }
  }
}

InverseLinftyOneRow <- function ( sigma, i, mu, maxiter=50, threshold=1e-2 ) {
  p <- nrow(sigma);
  rho <- max(abs(sigma[i,-i])) / sigma[i,i];
  mu0 <- rho/(1+rho);
  beta <- rep(0,p);

  if (mu >= mu0){
    beta[i] <- (1-mu0)/sigma[i,i];
    returnlist <- list("optsol" = beta, "iter" = 0);
    return(returnlist);
  }

  diff.norm2 <- 1;
  last.norm2 <- 1;
  iter <- 1;
  iter.old <- 1;
  beta[i] <- (1-mu0)/sigma[i,i];
  beta.old <- beta;
  sigma.tilde <- sigma;
  diag(sigma.tilde) <- 0;
  vs <- -sigma.tilde%*%beta;

  while ((iter <= maxiter) && (diff.norm2 >= threshold*last.norm2)){

    for (j in 1:p){
      oldval <- beta[j];
      v <- vs[j];
      if (j==i)
        v <- v+1;
      beta[j] <- SoftThreshold(v,mu)/sigma[j,j];
      if (oldval != beta[j]){
        vs <- vs + (oldval-beta[j])*sigma.tilde[,j];
      }
    }

    iter <- iter + 1;
    if (iter==2*iter.old){
      d <- beta - beta.old;
      diff.norm2 <- sqrt(sum(d*d));
      last.norm2 <-sqrt(sum(beta*beta));
      iter.old <- iter;
      beta.old <- beta;
      if (iter>10)
        vs <- -sigma.tilde%*%beta;
    }
  }

  returnlist <- list("optsol" = beta, "iter" = iter)
  return(returnlist)
}

InverseLinfty <- function(sigma, n, resol=1.5, mu=NULL, maxiter=50, threshold=1e-2, verbose = TRUE) {
  isgiven <- 1;
  if (is.null(mu)){
    isgiven <- 0;
  }

  p <- nrow(sigma);
  M <- matrix(0, p, p);
  xperc = 0;
  xp = round(p/10);
  for (i in 1:p) {
    if ((i %% xp)==0){
      xperc = xperc+10;
      if (verbose) {
        print(paste(xperc,"% done",sep="")); }
    }
    if (isgiven==0){
      mu <- (1/sqrt(n)) * qnorm(1-(0.1/(p^2)));
    }
    mu.stop <- 0;
    try.no <- 1;
    incr <- 0;
    while ((mu.stop != 1)&&(try.no<10)){
      last.beta <- beta
      output <- InverseLinftyOneRow(sigma, i, mu, maxiter=maxiter, threshold=threshold)
      beta <- output$optsol
      iter <- output$iter
      if (isgiven==1){
        mu.stop <- 1
      }
      else{
        if (try.no==1){
          if (iter == (maxiter+1)){
            incr <- 1;
            mu <- mu*resol;
          } else {
            incr <- 0;
            mu <- mu/resol;
          }
        }
        if (try.no > 1){
          if ((incr == 1)&&(iter == (maxiter+1))){
            mu <- mu*resol;
          }
          if ((incr == 1)&&(iter < (maxiter+1))){
            mu.stop <- 1;
          }
          if ((incr == 0)&&(iter < (maxiter+1))){
            mu <- mu/resol;
          }
          if ((incr == 0)&&(iter == (maxiter+1))){
            mu <- mu*resol;
            beta <- last.beta;
            mu.stop <- 1;
          }
        }
      }
      try.no <- try.no+1
    }
    M[i,] <- beta;
  }
  return(M)
}

NoiseSd <- function( yh, A, n ){
  ynorm <- sqrt(n)*(yh/sqrt(diag(A)));
  sd.hat0 <- mad(ynorm);

  zeros <- (abs(ynorm)<3*sd.hat0);
  y2norm <- sum(yh[zeros]^2);
  Atrace <- sum(diag(A)[zeros]);
  sd.hat1 <- sqrt(n*y2norm/Atrace);

  ratio <- sd.hat0/sd.hat1;
  if (max(ratio,1/ratio)>2)
    print("Warning: Noise estimate problematic");

  s0 <- sum(zeros==FALSE);
  return (list( "sd" = sd.hat1, "nz" = s0));
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

SSLasso <- function (X, y, alpha=0.05, lambda = NULL, mu = NULL, intercept = TRUE,
                     resol=1.3, maxiter=50, threshold=1e-2, verbose = TRUE) {
  #
  # Compute confidence intervals and p-values.
  #
  # Args:
  #   X     :  design matrix
  #   y     :  response
  #   alpha :  significance level
  #   lambda:  Lasso regularization parameter (if null, fixed by sqrt lasso)
  #   mu    :  Linfty constraint on M (if null, searches)
  #   resol :  step parameter for the function that computes M
  #   maxiter: iteration parameter for computing M
  #   threshold : tolerance criterion for computing M
  #   verbose : verbose?
  #
  # Returns:
  #   noise.sd: Estimate of the noise standard deviation
  #   norm0   : Estimate of the number of 'significant' coefficients
  #   coef    : Lasso estimated coefficients
  #   unb.coef: Unbiased coefficient estimates
  #   low.lim : Lower limits of confidence intervals
  #   up.lim  : upper limit of confidence intervals
  #   pvals   : p-values for the coefficients
  #
  p <- ncol(X);
  n <- nrow(X);
  pp <- p;
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  X <- X %*% diag(col.norm);

  htheta <- Lasso (X,y,lambda=lambda,intercept=intercept);

  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
  } else {
    Xb <- X;
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);

  if ((n>=2*p)){
    tmp <- eigen(sigma.hat)
    tmp <- min(tmp$values)/max(tmp$values)
  }else{
    tmp <- 0
  }

  if ((n>=2*p)&&(tmp>=1e-4)){
    M <- solve(sigma.hat)
  }else{
    M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
  }

  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n);

  A <- M %*% sigma.hat %*% t(M);
  projection<-M%*%t(Xb)
  noise <- NoiseSd( unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  Cov <- s.hat^2*A/n

  #sd.vector <- s.hat*sqrt(diag(A))/(sqrt(n));
  sd.vector <- sqrt(diag(Cov));
  #if  (is.null(lambda)){
  #  lambda <- s.hat*sqrt(qnorm(1-(0.1/p))/n);
  #}

  #addlength <- rep(0,pp);
  #MM <- M%*%sigma.hat - diag(pp);
  #for (i in 1:pp){
  #	effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
  #	effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
  #	addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  #}

  htheta <- htheta*col.norm;
  unbiased.Lasso <- unbiased.Lasso*col.norm;
  sd.vector <- sd.vector*col.norm;
  projection<-diag(col.norm)%*%projection
  #addlength <- addlength*col.norm;

  #if (intercept==TRUE){
  #  unbiased.Lasso <- unbiased.Lasso[2:pp];
  #  sd.vector <- sd.vector[2:pp];
  #}
  #p.vals <- 2*(1-pnorm(sqrt(n)*abs(unbiased.Lasso)/(s.hat*col.norm[(pp-p+1):pp]*sqrt(diag(A[(pp-p+1):pp,(pp-p+1):pp])))))

  returnList <- list("noise.sd" = s.hat,
                     "norm0" = noise$nz,
                     "coef" = htheta,
                     "unb.coef" = unbiased.Lasso,
                     "size" = sd.vector,
                     "proj"=projection,
                     "Cov"=Cov
  )
  return(returnList)
}

SSLasso.known <- function (X, y, design.cov=NULL,alpha=0.05, lambda = NULL, mu = NULL, intercept = TRUE,resol=1.3, maxiter=50, threshold=1e-2, verbose = TRUE) {
  #
  # Compute confidence intervals and p-values.
  #
  # Args:
  #   X     :  design matrix
  #   y     :  response
  #   design.cov: covariance matrix of the design X
  #   alpha :  significance level
  #   lambda:  Lasso regularization parameter (if null, fixed by sqrt lasso)
  #   mu    :  Linfty constraint on M (if null, searches)
  #   resol :  step parameter for the function that computes M
  #   maxiter: iteration parameter for computing M
  #   threshold : tolerance criterion for computing M
  #   verbose : verbose?
  #
  # Returns:
  #   noise.sd: Estimate of the noise standard deviation
  #   norm0   : Estimate of the number of 'significant' coefficients
  #   coef    : Lasso estimated coefficients
  #   unb.coef: Unbiased coefficient estimates
  #   low.lim : Lower limits of confidence intervals
  #   up.lim  : upper limit of confidence intervals
  #   pvals   : p-values for the coefficients
  #
  p <- ncol(X);
  n <- nrow(X);
  pp <- p;
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  X <- X %*% diag(col.norm);

  htheta <- Lasso (X,y,lambda=lambda,intercept=intercept);

  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
    sigma.known <- diag(col.norm)%*%design.cov%*%diag(col.norm)
  } else {
    Xb <- X;
    sigma.known <- diag(col.norm)%*%design.cov%*%diag(col.norm)
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);

  # if ((n>=2*p)){
  # tmp <- eigen(sigma.hat)
  # tmp <- min(tmp$values)/max(tmp$values)
  # }else{
  # tmp <- 0
  # }

  # if ((n>=2*p)&&(tmp>=1e-4)){
  #
  # }else{
  # M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
  # }
  M <- solve(sigma.known)
  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n);

  A <- M %*% sigma.hat %*% t(M);
  projection<-M%*%t(Xb)
  noise <- NoiseSd( unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  Cov <- s.hat^2*A/n

  #sd.vector <- s.hat*sqrt(diag(A))/(sqrt(n));
  sd.vector <- sqrt(diag(A));
  #if  (is.null(lambda)){
  #  lambda <- s.hat*sqrt(qnorm(1-(0.1/p))/n);
  #}

  #addlength <- rep(0,pp);
  #MM <- M%*%sigma.hat - diag(pp);
  #for (i in 1:pp){
  #	effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
  #	effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
  #	addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  #}

  htheta <- htheta*col.norm;
  Cov <- diag(col.norm)%*%Cov%*%diag(col.norm);
  unbiased.Lasso <- unbiased.Lasso*col.norm;
  sd.vector <- sd.vector*col.norm;
  projection<-diag(col.norm)%*%projection

  #if (intercept==TRUE){
  #  unbiased.Lasso <- unbiased.Lasso[2:pp];
  #  sd.vector <- sd.vector[2:pp];
  #}
  returnList <- list("unb.coef" = unbiased.Lasso,
                     "Cov"=Cov,
                     "noise.sd" = s.hat,
                     "norm0" = noise$nz,
                     "coef" = htheta,
                     "size" = sd.vector,
                     "proj"=projection
  )
  return(returnList)
}



####################The algorithm for treatment selection
############Inputs for the algorithm of treatment selection
#### for the first treatment, y denotes the outcome and X denotes the covariates matrix (without containing intercept)
####  for the second treatment, w denotes the outcome and Z denotes the covariates matrix (without containing intercept)
### xnew is a p-dimensional covariates for the new observation
### alpha is the significance level
### semi=FALSE denotes there is no unlabelled data
### intercept=TRUE denotes the fitted model includes intercept
#############Outputs for the algorithm of treatment selection
### "point": the point estimator of the treatment difference
### "sd": the standard error of the point estimator
### "CI": confidence interval for the treatment difference
### "decision": if decision=1, it means the first treatment is selected for the new
### observation compared to the second treatment
Treatment.sel<-function(X,y,Z,w,xnew, alpha=0.05,semi=FALSE, intercept=TRUE){
  if(semi==FALSE){
    DeLasso.A<-SSLasso(X,y,intercept)
    DeLasso.B<-SSLasso(Z,w,intercept)
  }else{
    Cov.est<-t(X.all)%*%X.all/N
    DeLasso.A<-SSLasso.known(X,y,design.cov=Cov.est,intercept)
    DeLasso.B<-SSLasso.known(Z,w,design.cov=Cov.est,intercept)
  }
  if(intercept==TRUE){
    p=length(xnew)
    loading=rep(0,p+1)
    loading[1]=1
    loading[-1]=xnew
  }
  coef.A<-DeLasso.A$unb.coef
  coef.B<-DeLasso.B$unb.coef
  Cov.A<-DeLasso.A$Cov
  Cov.B<-DeLasso.B$Cov
  vec<-coef.A-coef.B
  cov<-(Cov.A+Cov.B)
  diff.point<-sum(loading*vec)
  diff.sd<-sqrt(loading%*%cov%*%loading)
  diff.ci<-c(diff.point-qnorm(1-alpha/2)*diff.sd,diff.point+qnorm(1-alpha/2)*diff.sd)
  diff.decision<-(diff.point>qnorm(1-alpha)*diff.sd)
  returnList <- list("point" = diff.point,
                     "sd" = diff.sd,
                     "CI" = diff.ci,
                     "decision" = diff.decision,
                     "coef.A"=coef.A,
                     "coef.B"=coef.B,
                     "Cov.A"=Cov.A,
                     "Cov.B"=Cov.B
  )
  return(returnList)
}









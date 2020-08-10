getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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

#Lasso <- function( X, y, lambda = NULL, intercept = TRUE){
  #
  # Compute the Lasso estimator:
  # - If lambda is given, use glmnet and standard Lasso
  # - If lambda is not given, use square root Lasso
  #
#  p <- ncol(X);
#  n <- nrow(X);

#  if  (is.null(lambda)){
#    lambda <- sqrt(qnorm(1-(0.1/p))/n);
#    outLas <- slim(X,y,lambda=c(lambda),method="lq",q=2,verbose=FALSE);
    # Objective : sqrt(RSS/n) +lambda *penalty
#    if (intercept==TRUE) {
#      return (c(as.vector(outLas$intercept),as.vector(outLas$beta)))
#   }  else {
#      return (as.vector(outLas$beta));
#    }
#  } else {
 #   outLas <- glmnet(X, y, family = c("gaussian"), alpha =1, intercept = intercept );
    # Objective :1/2 RSS/n +lambda *penalty
#    if (intercept==TRUE){
#      return (as.vector(coef(Las,s=lambda)));
#    } else {
#      return (as.vector(coef(Las,s=lambda))[2:(p+1)]);
#    }
#  }
#}

Initialization.step<-function(X,y,lambda=NULL,intercept=FALSE){
  p <- ncol(X);
  n <- nrow(X);
  ### implement Lasso
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  Xnor <- X %*% diag(col.norm);
  htheta <- Lasso (Xnor,y,lambda=lambda,intercept=intercept);
  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),Xnor);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
  }else {
    Xb <- Xnor;
    pp <- p
  }
  sparsity<-sum(abs(htheta)>0.001)
  sd.est<-sum((y-Xb%*%htheta)^2)/(n-sparsity)
  htheta <- htheta*col.norm;
  returnList <- list("lasso.est" = htheta,
                     "sigma"=sd.est,
                     "sparsity"=sparsity)
  return(returnList)
}


Direction_fixedtuning_norm_lin<-function(Wc,test.vec,mu=NULL){
  pp<-ncol(Wc)
  n<-nrow(Wc)
  test.norm=sqrt(sum(test.vec^2))
  if(is.null(mu)){
    mu<-sqrt(2.01*log(pp)/n)
  }
  v<-Variable(pp)
  obj<-1/4*sum((Wc%*%v)^2)/n+sum((test.vec/test.norm)*v)+mu*sum(abs(v))
  prob<-Problem(Minimize(obj))
  result<-solve(prob)
  print("fixed mu")
  print(mu)
  print(result$value)
  opt.sol<-result$getValue(v)
  cvxr_status<-result$status
  direction<-(-1)/2*opt.sol
  returnList <- list("proj"=direction)
  return(returnList)
}

Direction_searchtuning_norm_lin<-function(Wc,test.vec,mu=NULL, resol, maxiter){
  pp<-ncol(Wc)
  n<-nrow(Wc)
  test.norm=sqrt(sum(test.vec^2))
  tryno = 1;
  opt.sol = rep(0,pp);
  lamstop = 0;
  cvxr_status = "optimal";
  ratio<-3
  mu = sqrt(2.01*log(pp)/n);
  #mu.initial= mu;

  while (lamstop == 0 && tryno < maxiter){
    print(tryno)
    #print(mu);
    lastv = opt.sol;
    lastresp = cvxr_status;
    v<-Variable(pp)
    obj<-1/4*sum((Wc%*%v)^2)/n+sum((test.vec/test.norm)*v)+mu*sum(abs(v))
    prob<-Problem(Minimize(obj))
    result<-solve(prob)
    opt.sol<-result$getValue(v)
    cvxr_status<-result$status
    if(tryno==1){### check whether this is the first iteration
      if(cvxr_status=="optimal"){### check whether the current tuning paramter solves the problem
        initial.sd<-sqrt(sum((Wc%*%opt.sol)^2)/(n)^2)*test.norm
        temp.sd<-sqrt(sum((Wc%*%opt.sol)^2)/(n)^2)*test.norm
        print(initial.sd)
        #print(temp.var)
        incr = 0;
        mu=mu/resol;
      }else{
        incr = 1;
        mu=mu*resol;
      }
    }else{
      if(incr == 1){ ### if the tuning parameter is increased in the last step
        if(cvxr_status=="optimal"){
          lamstop = 1;
        }else{
          mu=mu*resol;
        }
      }else{
        if((cvxr_status=="optimal")&& ((temp.sd)<ratio*(initial.sd)) ){
          mu = mu/resol;
          temp.sd<-sqrt(sum((Wc%*%opt.sol)^2)/(n)^2)*test.norm
          print(temp.sd)
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
  direction<-(-1)/2*opt.sol
  step<-tryno-1
  print(step)
  returnList <- list("proj"=direction,
                     "step"=step)
  return(returnList)
}



#' Inference for quadratic functional in high-dimensional linear regression model
#'
#' @description Computes the bias-corrected estimator of \eqn{\left\|\beta_G\right\|_{2}^{2}} for the following high-dimensional
#' linear regression model \deqn{y=X\beta+\epsilon}
#' as \deqn{\widehat{\left\|\beta_G\right\|_{2}^{2}}=\left\|\widehat{\beta_G}\right\|_{2}^{2}+\frac{2}{n} \widehat{u}^{\top} X^{\top}(y-X \widehat{\beta})}
#' where \eqn{G\subset{1,2,...,p}}
#'
#' @param X Design matrix, of dimension nobs(n) x nvar(p)
#' @param y Response variable
#' @param test.set set of indices, G in \eqn{\beta_G^{\top}\Sigma_{G,G}\beta_G}
#' @param tau.vec Vector of enlargement factors for asymptotic variance of the estimator \eqn{\widehat{\beta_G^{\top}\Sigma_{G,G}\beta_G}} to handle super-efficiency
#' @param lambda Tuning parameter \eqn{\lambda} used in construction of initial LASSO estimator \eqn{\widehat{\beta}} if \code{init.Lasso = NULL}
#' @param intercept Should intercept(s) be fitted (default = \code{FALSE})
#' @param mu Tuning parameter \eqn{\mu} in construction of projection direction \eqn{\widehat{u}}
#' @param step Number of steps (< \code{maxiter}) to obtain the smallest \code{mu} that gives convergence of the
#' optimization problem for constructing the projection direction \eqn{\widehat{u}} (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction \eqn{\widehat{u}}
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction \eqn{\widehat{u}}
#'
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator \eqn{\widehat{\left\|\beta_G\right\|_{2}^{2}}}}
#' \item{sigma}{Estimate of the variance of error term \eqn{\epsilon} in the linear regression model}
#' \item{se}{Standard error of \eqn{\widehat{\left\|\beta_G\right\|_{2}^{2}}}}
#' \item{plug.in}{Plug-in LASSO estimator \eqn{\left\|\widehat{\beta}_G\right\|_{2}^{2}}}
#'
#' @export
#'
#' @importFrom stats coef qnorm
#' @import CVXR Matrix glmnet
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' beta = (1:400)/25
#' y = X%*%beta + rnorm(100,0,1)
#' Group_Test_Norm(X = X, y = y, test.set=c(30:50))
Group_Test_Norm<-function(X,y,test.set,tau.vec=NULL,lambda=NULL,intercept=FALSE,mu=NULL,step=NULL,resol = 1.5,maxiter=10){
  p=ncol(X)
  n=nrow(X)
  ####### implement a lasso algorithm to get beta and sigma
  Ini.Est<-Initialization.step(X,y,lambda,intercept)
  htheta<-Ini.Est$lasso.est
  sd.est<-Ini.Est$sigma
  spar.est<-Ini.Est$sparsity
  ####### implement the correction of the initial estimator
  ####### set up the randomization step
  if (intercept==TRUE){
    Xc <- cbind(rep(1,n),X);
    pp <- (p+1);
    test.set<-test.set+1
  } else {
    Xc <- X;
    pp <- p
  }
  ### compute the initial estimator
  sigma.hat <- (1/n)*(t(Xc)%*%Xc);
  test.vec<-matrix(0,ncol=1,nrow=pp)
  test.vec[test.set]=htheta[test.set]
  lasso.plugin<-sum(test.vec^2)
  test.norm=sqrt(sum(test.vec^2))

  if(test.norm==0){
    direction<-rep(0,pp)
  }else{
    if ((n>=6*p)){
      tmp <- eigen(sigma.hat)
      tmp <- min(tmp$values)/max(tmp$values)
    }else{
      tmp <- 0
    }

    if ((n>=6*p)&&(tmp>=1e-4)){
      direction <- solve(sigma.hat)%*%test.vec
    }else{
      if(n>0.5*p){
        ### for option 1
        if(is.null(step)){
          step.vec<-rep(NA,3)
          for(t in 1:3){
            index.sel<-sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
            Direction.Est.temp<-Direction_searchtuning_norm_lin(Xc[index.sel,], test.vec,mu=NULL, resol, maxiter)
            step.vec[t]<-Direction.Est.temp$step
          }
          step<-getmode(step.vec)
        }
        print(paste("step is", step))
        Direction.Est<-Direction_fixedtuning_norm_lin(Xc, test.vec,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)})
      }else{
        ### for option 2
        Direction.Est<-Direction_searchtuning_norm_lin(Xc, test.vec,mu=NULL, resol, maxiter)
        step<-Direction.Est$step
        print(paste("step is", step))
      }
      direction<-Direction.Est$proj
    }
  }
  ####### correct the initial estimator by the constructed projection direction
  correction = 2*test.norm*t(Xc%*%direction)%*%(y - Xc%*%htheta)/n;
  debias.est=lasso.plugin+correction
  se<-2*sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*test.norm
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
  returnList <- list("prop.est" = debias.est,
                     "sigma"=sd.est,
                     "se" =se.vec,
                     "plug.in"=lasso.plugin
  )
  return(returnList)
}





































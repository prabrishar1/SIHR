getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



#####################################################################################################
################## plugin estimator

###### The following function computes the lasso estimator

# Compute the Lasso estimator:

# - If lambda is given, use glmnet and standard Lasso

# - If lambda is set to the character string "CV", then glmnet with

#   lambda selected by cross-validation is used

# - If lambda is not given or is set to NULL, use square root Lasso

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
#    }  else {
#      return (as.vector(outLas$beta));
#    }
#  } else {
#    outLas <- glmnet(X, y, family = c("gaussian"), alpha =1, intercept = intercept );
    # Objective :1/2 RSS/n +lambda *penalty
#    if (intercept==TRUE){
#      return (as.vector(coef(outLas,s=lambda)));
#    } else {
#      return (as.vector(coef(outLas,s=lambda))[2:(p+1)]);
#    }
#  }
#}


#' Constructs the projection direction with fixed tuning parameter for estimating functional in high-dimensional linear regression model
#'
#' @param Xc Design matrix
#' @param loading observation vector in the linear functional
#' @param mu Tuning parameter in construction of projection direction
#'
#' @return
#' \item{proj}{The projection direction}
#' @export
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' Direction_fixedtuning(X,loading=c(1,rep(0,399)),mu=2)
#'
Direction_fixedtuning<-function(Xc,loading,mu=NULL){
  pp<-ncol(Xc)
  n<-nrow(Xc)
  if(is.null(mu)){
    mu<-sqrt(2.01*log(pp)/n)
  }
  loading.norm<-sqrt(sum(loading^2))
  H<-cbind(loading/loading.norm,diag(1,pp))
  v<-Variable(pp+1)
  obj<-1/4*sum((Xc%*%H%*%v)^2)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
  prob<-Problem(Minimize(obj))
  result<-solve(prob)
  print("fixed mu")
  print(mu)
  print(result$value)
  opt.sol<-result$getValue(v)
  cvxr_status<-result$status
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  returnList <- list("proj"=direction)
  return(returnList)
}

#' Constructs the projection direction with "optimal" tuning parameter for estimating functional in high-dimensional linear regression model
#'
#' @param Xc Design matrix
#' @param loading observation vector in the linear functional
#' @param mu Tuning parameter in construction of projection direction
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction
#'
#' @return
#' \item{proj}{The projection direction}
#'
#' @export
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' Direction_searchtuning(X,loading=c(1,rep(0,399)),mu=NULL,resol=1.5,maxiter=10)
Direction_searchtuning<-function(Xc,loading,mu=NULL, resol, maxiter){
  pp<-ncol(Xc)
  n<-nrow(Xc)
  tryno = 1;
  opt.sol = rep(0,pp);
  lamstop = 0;
  cvxr_status = "optimal";

  mu = sqrt(2.01*log(pp)/n);
  #mu.initial= mu;
  while (lamstop == 0 && tryno < maxiter){
    ###### This iteration is to find a good tuning parameter
    #print(mu);
    lastv = opt.sol;
    lastresp = cvxr_status;
    loading.norm<-sqrt(sum(loading^2))
    H<-cbind(loading/loading.norm,diag(1,pp))
    v<-Variable(pp+1)
    obj<-1/4*sum((Xc%*%H%*%v)^2)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
    prob<-Problem(Minimize(obj))
    result<-solve(prob)
    #print(result$value)
    opt.sol<-result$getValue(v)
    cvxr_status<-result$status
    #print(cvxr_status)
    if(tryno==1){
      if(cvxr_status=="optimal"){
        incr = 0;
        mu=mu/resol;
        temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
        initial.sd<-sqrt(sum((Xc%*% temp.vec)^2)/(n)^2)*loading.norm ##what's this?
        temp.sd<-initial.sd
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
        if(cvxr_status=="optimal"&&temp.sd<3*initial.sd){ ##Why this condition on sd?
          mu = mu/resol;
          temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
          temp.sd<-sqrt(sum((Xc%*% temp.vec)^2)/(n)^2)*loading.norm
          #print(temp.sd)
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

#' Inference for linear functional in the high-dimensional linear regression model
#'
#' @description
#' Computes the bias corrected estimator of linear functional for the high-dimensional linear regression model and the corresponding standard error.
#'
#' @param X Design matrix
#' @param y Response variable
#' @param loading observation vector in the linear functional
#' @param init.Lasso initial LASSO estimator of the regression vector (default = \code{NULL})
#' @param lambda Tuning parameter in construction of LASSO estimator of the regression vector (default = \code{NULL})
#' @param intercept Should intercept(s) be fitted (default = \code{FALSE})
#' @param mu Tuning parameter in construction of projection direction (default = \code{NULL})
#' @param step Number of steps (< \code{maxiter}) to obtain the smallest \code{mu} that gives convergence of the
#' optimization problem for constructing the projection direction (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' that gives convergence of the optimization problem for constructing the projection direction (default = 10)
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator for the linear functional}
#' \item{sigma}{Estimate of the error variance in the linear regression model}
#' \item{se}{Standard error of the bias-corrected estimator}
#' \item{proj}{Optimal projection direction}
#' \item{step}{Number of steps (< \code{maxiter}) to obtain the smallest \code{mu} that gives convergence of the
#' optimization problem for constructing the projection direction}
#' \item{plug.in}{Plug-in LASSO estimator for the linear functional}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef qnorm
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlin}{FIHR}
#'
#' @examples
#' X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400)
#' beta = (1:400)/25
#' y = X%*%beta + rnorm(100,0,1)
#' LF(X = X, y = y, loading = c(1,rep(0,399)), intercept = TRUE)
LF<-function(X,y,loading,init.Lasso=NULL,lambda=NULL,intercept=FALSE,mu=NULL,step=NULL,resol = 1.5,maxiter=10){
  ### Option 1: search tuning parameter with steps determined by the ill conditioned case (n=p/2)
  ### Option 2: search tuning parameter with maximum 10 steps.
  ####### Option 3: fixed tuning parameter and this is not recommended without exploring the tuning parameter selection
  xnew<-loading
  p <- ncol(X);
  n <- nrow(X);

  if(is.null(init.Lasso)){
    ####### implement a lasso algorithm to get beta and sigma
    Ini.Est<-Initialization.step(X,y,lambda,intercept)
  }
  htheta<-Ini.Est$lasso.est
  sd.est<-Ini.Est$sigma
  spar.est<-Ini.Est$sparsity
  ####### implement the correction of the initial estimator
  ####### set up the randomization step
  if (intercept==TRUE){
    Xc <- cbind(rep(1,n),X);
    pp <- (p+1);
  } else {
    Xc <- X;
    pp <- p
  }

#  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));

#  Xnor <- X %*% diag(col.norm);
  ### implement Lasso
#  htheta <- Lasso (Xnor,y,lambda=lambda,intercept=intercept);
#  if (intercept==TRUE){
#    Xb <- cbind(rep(1,n),Xnor);
#    Xc <- cbind(rep(1,n),X);
#    col.norm <- c(1,col.norm);
#    pp <- (p+1);
#  } else {
#    Xb <- Xnor;
#    Xc <- X;
#    pp <- p
#  }
#  sparsity<-sum(abs(htheta)>0.001)
#  sd.est<-sum((y-Xb%*%htheta)^2)/(n-sparsity)
#  htheta <- htheta*col.norm;


    ### compute the initial estimator
  if(intercept==TRUE){
    loading=rep(0,pp)
    loading[1]=1
    loading[-1]=xnew
  }else{
    loading=xnew
  }
  loading.norm<-sqrt(sum(loading^2))
  lasso.plugin<-sum(loading*htheta)


  #####################################################################################################
  ################## Correction step


  if ((n>=6*p)){
    sigma.hat <- (1/n)*(t(Xc)%*%Xc);
    tmp <- eigen(sigma.hat)
    tmp <- min(tmp$values)/max(tmp$values)
  }else{
    tmp <- 0
  }
  sigma.hat <- (1/n)*(t(Xc)%*%Xc);
  if ((n>=6*p)&&(tmp>=1e-4)){
    direction <- solve(sigma.hat)%*%loading
  }else{
    if(n>0.5*p){
      ### for option 1
      if(is.null(step)){
        step.vec<-rep(NA,3)
        for(t in 1:3){
          index.sel<-sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
          Direction.Est.temp<-Direction_searchtuning(Xc[index.sel,],loading,mu=NULL, resol, maxiter)
          step.vec[t]<-Direction.Est.temp$step
        }
        step<-getmode(step.vec)
      }
      print(paste("step is", step))
      Direction.Est<-Direction_fixedtuning(Xc,loading,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)})
    }else{
      ### for option 2
      Direction.Est<-Direction_searchtuning(Xc,loading,mu=NULL, resol, maxiter)
      step<-Direction.Est$step
      print(paste("step is", step))
    }
    direction<-Direction.Est$proj
  }
  correction = t(Xc%*%direction)%*%(y - Xc%*%htheta)/n;
  debias.est=lasso.plugin+correction*loading.norm
  #cbind(true,linear.plugin,linear.plugin+correct,correct)
  se<-sd.est*sqrt(sum((Xc%*%direction)^2)/(n)^2)*loading.norm
  #sd
  #c(linear.plugin+correct-1.96*sd,linear.plugin+correct+1.96*sd)
  returnList <- list("prop.est" = debias.est,
                     "sigma"=sd.est,
                     "se" = se,
                     "proj"=direction,
                     "step"=step,
                     "plug.in"=lasso.plugin
  )
  return(returnList)
}




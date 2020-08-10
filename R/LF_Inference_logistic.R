getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Direction_fixedtuning_logistic<-function(Xc,loading,mu=NULL){
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
  #print(result$value)
  opt.sol<-result$getValue(v)
  cvxr_status<-result$status
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  returnList <- list("proj"=direction)
  return(returnList)
}

Direction_searchtuning_logistic<-function(Xc,loading,mu=NULL, resol, maxiter){
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
        initial.sd<-sqrt(sum((Xc%*% temp.vec)^2)/(n)^2)*loading.norm
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
        if(cvxr_status=="optimal"&&temp.sd<3*initial.sd){
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

#' Inference for linear functional in high dimensional logistic regression
#'
#' @param X input matrix, of dimension nobs x nvar; each row is an observation vector
#' @param y response variable
#' @param loading the vector of covariates in the linear functional to be estimated
#' @param lambda tuning parameter
#' @param intercept Should intercept(s) be fitted (default = \code{FALSE})
#' @param mu tuning parameter for construction of projection direction
#' @param step number of steps (< \code{maxiter}) to obtain the smallest \code{mu} that gives convergence
#' @param resol resolution or the factor by which \code{mu} is increased/decreased till smallest with convergence
#' @param maxiter maximum number of steps along which we increase/decrease \code{mu} till smallest with convergence
#'
#' @return bias corrected linear  functional estimate and corresponding standard error for high dimensional logistic regression model
#' @export
#'
#' @importFrom stats coef
#' @import CVXR AER Matrix glmnet
#'
#' @examples
#' LF_Inference_logistic(X = matrix(sample(-2:2,100*400,replace = TRUE),nrow=100,ncol=400),
#'                       y = rbinom(100,1,0.5), loading = c(1,rep(0,399)), intercept = TRUE)
LF_Inference_logistic<-function(X,y,loading,lambda=NULL,intercept=FALSE,mu=NULL,step=NULL,resol = 1.5,maxiter=6){
  ### Option 1: search tuning parameter with steps determined by the ill conditioned case (n=p/2)
  ### Option 2: search tuning parameter with maximum 10 steps.
  ####### Option 3: fixed tuning parameter and this is not recommended without exploring the tuning parameter selection
  xnew<-loading
  X<-as.matrix(X)
  p <- ncol(X);
  n <- nrow(X);
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X)+0.0001);

  Xnor <- X %*% diag(col.norm);
  ### implement Lasso
  fit = cv.glmnet(Xnor, y, alpha=1,family = "binomial")
  htheta <- as.vector(coef(fit, s = "lambda.min"))
  support<-(abs(htheta)>0.001)
  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),Xnor);
    Xc <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
  } else {
    Xb <- Xnor;
    Xc <- X;
    pp <- p
  }
  #sparsity<-sum(abs(htheta)>0.001)
  #sd.est<-sum((y-Xb%*%htheta)^2)/(n-sparsity)
  htheta <- htheta*col.norm;
  htheta <- as.vector(htheta)
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

  if ((n>=6*p)&&(tmp>=1e-4)){
    direction <- solve(sigma.hat)%*%loading
  }else{
    if(n>0.5*p){
      ### for option 1
      if(is.null(step)){
        step.vec<-rep(NA,3)
        for(t in 1:3){
          index.sel<-sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
          Direction.Est.temp<-Direction_searchtuning_logistic(Xc[index.sel,],loading,mu=NULL, resol, maxiter)
          step.vec[t]<-Direction.Est.temp$step
        }
        step<-getmode(step.vec)
      }
      print(paste("step is", step))
      Direction.Est<-Direction_fixedtuning_logistic(Xc,loading,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)})
    }else{
      ### for option 2
      Direction.Est<-Direction_searchtuning_logistic(Xc,loading,mu=NULL, resol, maxiter)
      step<-Direction.Est$step
      print(paste("step is", step))
    }
    direction<-Direction.Est$proj
  }
  exp_pred=Xc%*%(htheta)
  weighed.residual=(y - exp(exp_pred)/(1+ exp(exp_pred)))*(1+exp(exp_pred))^2/exp(exp_pred)
  #exp_val=a0+X%*%beta
  #weighed.residual.ora=(y - exp(exp_val)/(1+ exp(exp_val)))*(1+exp(exp_val))^2/exp(exp_val)

  correction = sum((Xc%*%direction)*weighed.residual)/n;
  debias.est=lasso.plugin+correction*loading.norm
  #cbind(true,linear.plugin,linear.plugin+correct,correct)
  #se<-sqrt(sum((Xc%*%direction*(1+exp(exp_pred))/exp(exp_pred/2))^2)/(n)^2)*loading.norm*sum((y - exp(exp_pred)/(1+ exp(exp_pred)))^2)/n
  #se<-sqrt(mean((Xc%*%direction)^2*weighed.residual^2))*loading.norm/sqrt(n)
  #se.ora<-sqrt(mean((Xc%*%direction)^2*weighed.residual.ora^2))*loading.norm/sqrt(n)
  #se.another<-sqrt(mean((Xc%*%direction)^2*(1+exp(exp_pred))^2/exp(exp_pred)))*loading.norm/sqrt(n)
  se<-sqrt(mean((Xc%*%direction)^2*(1+exp(exp_pred))^2/exp(exp_pred)))*loading.norm/sqrt(n)
  #sd
  #mean((y - exp(exp_pred)/(1+ exp(exp_pred)))^2)
  #mean((y - exp(exp_val)/(1+ exp(exp_val)))^2)
  #mean(exp(exp_pred)/(1+ exp(exp_pred))^2)
  #c(linear.plugin+correct-1.96*sd,linear.plugin+correct+1.96*sd)
  returnList <- list("prop.est" = debias.est,
                     "se" = se,
                     "proj"=direction,
                     "step"=step,
                     "plug.in"=lasso.plugin,
                     "support"=support
  )
  return(returnList)
}

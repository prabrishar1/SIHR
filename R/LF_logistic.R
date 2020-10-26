getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#f_prime <- function(x)
#{
#  g = exp(x)/(1+exp(x))^2
#  return(g)
#}

Initialization.step <- function(X, y, lambda = NULL, intercept = FALSE) {
  n <- nrow(X)
   col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
  #col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2))
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
  #sparsity <- sum(abs(htheta) > 0.001)
  #sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / n)
  htheta <- htheta * col.norm
  returnList <- list("lasso.est" = htheta)
  #                   "sigma" = sd.est,
  #                   "sparsity" = sparsity)
  return(returnList)
}

Lasso <- function(X, y, lambda = NULL, intercept = TRUE) {
  p <- ncol(X)
  n <- nrow(X)

  htheta <- if (is.null(lambda)) {
    outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                        intercept = intercept)
    # Objective : 1/2 * RSS/n + lambda * penalty
    as.vector(coef(outLas, s = outLas$lambda.min))
  } else if (lambda == "CV") {
    outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                        intercept = intercept)
    # Objective : 1/2 * RSS/n + lambda * penalty
    as.vector(coef(outLas, s = outLas$lambda.1se))
  } else {
    outLas <- glmnet(X, y, family = "binomial", alpha = 1,
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

Direction_fixedtuning_logistic<-function(X,loading,mu=NULL,weight,deriv.vec){      ####### included functions weight and deriv.vec
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

  #H<-cbind(loading/loading.norm,diag(1,pp))
  v<-Variable(pp+1)



  #obj<-t(v)%*%Gamma%*%v +sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))   #######modified

  obj<-1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))   #######modified
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

Direction_searchtuning_logistic<-function(X,loading,mu=NULL,weight,deriv.vec,resol=1.5, maxiter=10){     #included weight and deriv.vec
  pp<-ncol(X)
  n<-nrow(X)
  tryno = 1;
  opt.sol = rep(0,pp+1);
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

    if (loading.norm==0){
      H <- cbind(loading, diag(1, pp))
    }else{
      H <- cbind(loading / loading.norm, diag(1, pp))
    }

    #H<-cbind(loading/loading.norm,diag(1,pp))
    v<-Variable(pp+1)
    obj<-1/4*sum((X%*%H%*%v)^2)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
    obj<-1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))    #######modified
    prob<-Problem(Minimize(obj))
    result<-solve(prob)
    #print(result$value)
    #opt.sol<-result$getValue(v)
    cvxr_status<-result$status
    #print(cvxr_status)
    if(tryno==1){
      if(cvxr_status=="optimal"){
        incr = 0;
        mu=mu/resol;

        opt.sol<-result$getValue(v)

        temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
        initial.sd<-sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm   ############modified
        temp.sd<-initial.sd
      }else{
        incr = 1;
        mu=mu*resol;
      }
    }else{
      if(incr == 1){ ### if the tuning parameter is increased in the last step
        if(cvxr_status=="optimal"){
          lamstop = 1;
          opt.sol <- result$getValue(v)

        }else{
          mu=mu*resol;
        }
      }else{
        if(cvxr_status=="optimal"&&temp.sd<3*initial.sd){
          mu = mu/resol;

          opt.sol <- result$getValue(v)

          temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
          temp.sd<-sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm     ############modified
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

#' Inference for linear functional in the high dimensional logistic regression
#'
#' @description
#' Computes the bias corrected estimator of the linear functional \code{loading}\eqn{^{\top}\beta} for the high dimensional logistic regression \eqn{Y_i|X_i \sim } Bernoulli\eqn{(\frac{e^{X_i^{\top}\beta}}{1+e^{X_i^{\top}\beta}})} and the corresponding standard error.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading Loading, of length \eqn{p}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param weight The weight vector, of length \eqn{n}, used in correcting the plug-in estimator, uses the inverse Hessian weight if set to \code{NULL} (default = \code{NULL})
#' @param init.Lasso Initial LASSO estimator of the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of LASSO estimator of the regression vector (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step Number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol Resolution or the factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 10)
#'
#' @return
#' \item{prop.est}{The bias corrected estimator of the linear functional}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator of the linear functional}
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlog}{FIHR}
#' @examples
#' A1gen <- function(rho,p){
#' A1=matrix(0,p,p)
#' for(i in 1:p){
#'   for(j in 1:p){
#'     A1[i,j]<-rho^(abs(i-j))
#'   }
#' }
#' A1
#' }
#' n = 100
#' p = 400
#' mu <- rep(0,p)
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' Cov2<-matrix(NA,nrow=p,ncol=p)
#' for(i in 1:p){
#'   for(j in 1:p){
#'     Cov2[i,j]<-0.5^(1+abs(i-j))
#'   }
#' }
#' beta <- rep(0,p)
#' beta[1:10] <- c(1:10)/5
#' X <- MASS::mvrnorm(n,mu,Cov)
#' exp_val <- X%*%beta
#' prob <- exp(exp_val)/(1+exp(exp_val))
#' y <- rbinom(n,1,prob)
#' loading <- MASS::mvrnorm(1,mu,Cov2)
#' Est <- LF_logistic(X = X, y = y, loading = loading, intercept = TRUE, weight = NULL)

LF_logistic<-function(X,y,loading,weight=NULL,intercept=TRUE,init.Lasso=NULL,lambda=NULL,mu=NULL,step=NULL,resol = 1.5,maxiter=10){

  ### included weight, deriv.vec to be constructed

  ### Option 1: search tuning parameter with steps determined by the ill conditioned case (n=p/2)
  ### Option 2: search tuning parameter with maximum 10 steps.
  ### Option 3: fixed tuning parameter and this is not recommended without exploring the tuning parameter selection
  xnew <- loading
  X<-as.matrix(X)
  p <- ncol(X);
  n <- nrow(X);
  ### implement Lasso
  #fit = cv.glmnet(Xnor, y, alpha=1,family = "binomial")
  #htheta <- as.vector(coef(fit, s = "lambda.min"))
  #support<-(abs(htheta)>0.001)
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
    col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
    #col.norm <- 1 / sqrt((1 / n) * diagXtX(X, MARGIN = 2));
    Xnor <- X %*% diag(col.norm);
    if(is.null(init.Lasso))
    {
      init.Lasso <- Initialization.step(X,y,lambda,intercept)
      htheta <- init.Lasso$lasso.est
    }
    else
    {
      htheta <- init.Lasso
    }


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

    sparsity <- sum(abs(htheta) > 0.001)
    sd.est <- sqrt(sum((y - Xb %*% htheta)^2) / max(0.9*n, n - sparsity))

    #sparsity<-sum(abs(htheta)>0.001)
    #sd.est<-sum((y-Xb%*%htheta)^2)/(n-sparsity)
    #htheta <- htheta*col.norm;
    #htheta <- as.vector(htheta)
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
    deriv.vec <- exp(Xc%*%htheta)/(1+exp(Xc%*%htheta))^2    ###### computed deriv.vec

    if(is.null(weight)){
      weight <- 1/deriv.vec
    }

    #####################################################################################################
    ################## Correction step


    if ((n>=6*p)){
      #sigma.hat <- (1/n)*(t(Xc)%*%Xc);
      gamma.hat <- (1/n)*sum((Xc^2)*weight*deriv.vec)   ##### computed gamma.hat instead of sigma.hat WRONG
      tmp <- eigen(gamma.hat)                         ##### computed eigen values of gamma.hat instead of sigma.hat
      tmp <- min(tmp$values)/max(tmp$values)
    }else{
      tmp <- 0
    }

    if ((n>=6*p)&&(tmp>=1e-4)){
      direction <- solve(gamma.hat)%*%loading         ##### computed direction as gamma.hat inverse loading
    }else{
#      if(n>0.5*p){
        ### for option 1
        if(is.null(step)){
          step.vec<-rep(NA,3)
          for(t in 1:3){
            index.sel<-sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
            Direction.Est.temp<-Direction_searchtuning_logistic(Xc[index.sel,],loading,mu=NULL,weight = weight[index.sel],deriv.vec = deriv.vec[index.sel],resol,maxiter)
            step.vec[t]<-Direction.Est.temp$step
          }
          step<-getmode(step.vec)
        }
        print(paste("step is", step))
        Direction.Est<-Direction_fixedtuning_logistic(Xc,loading,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)},weight = weight,deriv.vec = deriv.vec)

        while(is.na(Direction.Est)&&(step>0)){
          #print(paste("step is", step))
          step<-step-1
          Direction.Est<-Direction_fixedtuning_logistic(Xc,loading,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)},weight = weight,deriv.vec = deriv.vec)
        }
        #while(is.na(Direction.Est)&&(step>0)){
          #print(paste("step is", step))
        #  step<-step-1
        #  Direction.Est <- Direction_fixedtuning(Xc, test.vec, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
        #}
#      }else{
       ### for option 2
#        Direction.Est<-Direction_searchtuning_logistic(Xc,loading,mu=NULL,weight = weight,deriv.vec = deriv.vec,resol, maxiter)
#        step<-Direction.Est$step
#        proj <- Direction.Est$proj
#        while (sum(proj^2) < 10^(-3)) {
#          step <- step - 1
#          Direction.Est <- Direction_fixedtuning_logistic(Xc, loading, mu = sqrt(2.01 * log(pp) / n) * resol^{-(step - 1)})
#          proj <- Direction.Est$proj
#        }
#        print(paste("step is", step))
#      }
      direction<-Direction.Est$proj
    }
    exp_pred=Xc%*%(htheta)
    #weighed.residual=(y - exp(exp_pred)/(1+ exp(exp_pred)))*(1+exp(exp_pred))^2/exp(exp_pred)

    weighed.residual=(y - exp(exp_pred)/(1+ exp(exp_pred)))*weight   ##### modified weight
    #exp_val=a0+X%*%beta
    #weighed.residual.ora=(y - exp(exp_val)/(1+ exp(exp_val)))*(1+exp(exp_val))^2/exp(exp_val)

    correction = sum((Xc%*%direction)*weighed.residual)/n;
    debias.est=lasso.plugin+correction*loading.norm
    #cbind(true,linear.plugin,linear.plugin+correct,correct)
    #se<-sqrt(sum((Xc%*%direction*(1+exp(exp_pred))/exp(exp_pred/2))^2)/(n)^2)*loading.norm*sum((y - exp(exp_pred)/(1+ exp(exp_pred)))^2)/n
    #se<-sqrt(mean((Xc%*%direction)^2*weighed.residual^2))*loading.norm/sqrt(n)
    #se.ora<-sqrt(mean((Xc%*%direction)^2*weighed.residual.ora^2))*loading.norm/sqrt(n)
    #se.another<-sqrt(mean((Xc%*%direction)^2*(1+exp(exp_pred))^2/exp(exp_pred)))*loading.norm/sqrt(n)

    se<-sqrt(mean((Xc%*%direction)^2*weight^2*deriv.vec))*loading.norm/sqrt(n) ##### modified
    #sd
    #mean((y - exp(exp_pred)/(1+ exp(exp_pred)))^2)
    #mean((y - exp(exp_val)/(1+ exp(exp_val)))^2)
    #mean(exp(exp_pred)/(1+ exp(exp_pred))^2)
    #c(linear.plugin+correct-1.96*sd,linear.plugin+correct+1.96*sd)
    returnList <- list("prop.est" = debias.est,
                       "se" = se,
                       "proj"=direction,
                       "plug.in"=lasso.plugin
    )
    return(returnList)
  }
}

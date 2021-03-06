getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

expo <- function(z){
  g = exp(z)/(1+exp(z))
  return(g)
}

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
  htheta <- htheta * col.norm
  returnList <- list("lasso.est" = htheta)
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

Direction_fixedtuning_logistic<-function(X,loading,mu=NULL,weight,deriv.vec){
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

  obj<-1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
  prob<-Problem(Minimize(obj))
  result<-solve(prob)
  opt.sol<-result$getValue(v)
  cvxr_status<-result$status
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  returnList <- list("proj"=direction)
  return(returnList)
}

Direction_searchtuning_logistic<-function(X,loading,mu=NULL,weight,deriv.vec,resol=1.5, maxiter=6){     #included weight and deriv.vec
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
    obj<-1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))    #######modified
    prob<-Problem(Minimize(obj))
    result<-solve(prob)
    cvxr_status<-result$status
    if(tryno==1){
      if(cvxr_status=="optimal"){
        incr = 0;
        mu=mu/resol;

        opt.sol<-result$getValue(v)

        temp.vec<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
        initial.sd<-sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm
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
          temp.sd<-sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm
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
  returnList <- list("proj"=direction,
                     "step"=step)
  return(returnList)
}

#' Inference for the case probability in high dimensional logistic regression
#'
#' @description
#' Computes the bias corrected estimator of the case probability in the high dimensional logistic regression model and the corresponding standard error.
#' It also constructs the confidence interval for the case probability and test whether the case probability is above zero or not.
#' Here case probability refers to the conditional probability of the binary response variable taking value 1 given the predictors take value \code{loading}.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading Loading, of length \eqn{p}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param weight The weight vector used for bias correction, of length \eqn{n}; if set to \code{NULL}, the weight is the inverse of the first derivative of the logit function (default = \code{NULL})
#' @param init.Lasso Initial LASSO estimator of the regression vector (default = \code{NULL})
#' @param lambda The tuning parameter used in the construction of LASSO estimator of the regression vector (default = \code{NULL})
#' @param mu The dual tuning parameter used in the construction of the projection direction (default = \code{NULL})
#' @param step The step size used to compute \code{mu}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
#' such that the dual optimization problem for constructing the projection direction converges (default = 6)
#' @param alpha Level of significance to test the null hypothesis that the case probability is less than or equal to 0.5 (default = 0.05)
#'
#' @return
#' \item{prop.est}{The bias corrected estimator of the case probability}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the case probability}
#' \item{decision}{\code{decision}\eqn{=1} implies the case probability is above 0.5 \eqn{\newline}
#' \code{decision}\eqn{=0} implies the case probability is not above 0.5}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The plug-in LASSO estimator of the case probability}
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit
#' @import CVXR Matrix glmnet
#'
#' @references
#'
#' \insertRef{linlog}{SIHR}
#' @examples
#' A1gen <- function(rho,p){
#'  A1=matrix(0,p,p)
#'  for(i in 1:p){
#'    for(j in 1:p){
#'      A1[i,j]<-rho^(abs(i-j))
#'    }
#'  }
#'  A1
#' }
#' n = 100
#' p = 400
#' mu <- rep(0,p)
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta <- rep(0,p)
#' beta[1:10] <- 0.5*c(1:10)/10
#' a0 = 0
#' set.seed(12)
#' loading <- MASS::mvrnorm(1,mu,Cov)
#' X <- MASS::mvrnorm(n,mu,Cov)
#' exp_val <- X%*%beta+a0
#' prob <- exp(exp_val)/(1+exp(exp_val))
#' y <- rbinom(n,1,prob)
#' Est <- LF_logistic(X = X, y = y, loading = loading)
LF_logistic<-function(X,y,loading,weight=NULL,intercept=TRUE,init.Lasso=NULL,lambda=NULL,mu=NULL,step=NULL,resol = 1.5,maxiter=6, alpha = 0.05){
  xnew <- loading
  X<-as.matrix(X)
  p <- ncol(X);
  n <- nrow(X);
  n_y <- length(y)

  if(n_y!=n)
  {
    print("Check dimensions of X and y")
  }else{
    data = na.omit(data.frame(y,X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X);
    n <- nrow(X);
    col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X)+0.0001);
    Xnor <- X %*% diag(col.norm);
    if(is.null(init.Lasso))
    {
      init.Lasso <- Initialization.step(X,y,lambda,intercept)
      htheta <- init.Lasso$lasso.est
    }    else    {
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

    if(intercept==TRUE){
      loading=rep(0,pp)
      loading[1]=1
      loading[-1]=xnew
    }else{
      loading=xnew
    }
    loading.norm<-sqrt(sum(loading^2))
    lasso.plugin<-sum(loading*htheta)
    deriv.vec <- exp(Xc%*%htheta)/(1+exp(Xc%*%htheta))^2

    if(is.null(weight)){
      weight <- 1/deriv.vec
    }

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
      #####################################################################################################
      ################## Correction step


      if ((n>=6*p)){
        gamma.hat <- (1/n)*sum((Xc^2)*weight*deriv.vec)   ##### computed gamma.hat instead of sigma.hat WRONG
        tmp <- eigen(gamma.hat)
        tmp <- min(tmp$values)/max(tmp$values)
      }else{
        tmp <- 0
      }

      if ((n>=6*p)&&(tmp>=1e-4)){
        direction <- solve(gamma.hat)%*%loading
      }else{
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

        while(is.na(Direction.Est)&&(step>0) || length(Direction.Est$proj)==0&&(step>0)){
          step<-step-1
          Direction.Est<-Direction_fixedtuning_logistic(Xc,loading,mu=sqrt(2.01*log(pp)/n)*resol^{-(step-1)},weight = weight,deriv.vec = deriv.vec)
        }
        direction<-Direction.Est$proj
      }
      exp_pred=Xc%*%(htheta)

      weighed.residual=(y - exp(exp_pred)/(1+ exp(exp_pred)))*weight

      correction = sum((Xc%*%direction)*weighed.residual)/n;
      debias.est=lasso.plugin+correction*loading.norm
      rho_hat <- exp(loading%*%htheta)/(1+exp(loading%*%htheta))^2
      se_linear<-sqrt(mean((Xc%*%direction)^2*weight^2*deriv.vec))*loading.norm/sqrt(n)
      se <- se_linear*rho_hat
      CI <- c(debias.est - 2*qnorm(1-alpha/2)*se, debias.est + 2*qnorm(1-alpha/2)*se)
      if(debias.est - qnorm(1-alpha)*se > 0){
        dec <- 1
      }else{
        dec <- 0
      }

      returnList <- list("prop.est" = expo(debias.est),
                         "se" = se,
                         "CI" = c(expo(CI[1]),expo(CI[2])),
                         "decision" = dec,
                         "proj"=direction,
                         "plug.in"=expo(lasso.plugin)
      )
      return(returnList)
    }

  }
}

#' Inference for difference of case probabilities in high dimensional logistic regressions
#'
#' @description
#' Computes the bias corrected estimator of the difference between case probabilities with respect to two high dimensional logistic regression models
#' and the corresponding standard error. It also constructs the confidence interval for the difference of case probabilities and test
#' whether it is above zero or not. Here the case probability refers to the conditional probability of the binary response variable taking value 1 given the predictors are assigned to \code{loading}.
#'
#' @param X1 Design matrix for the first sample, of dimension \eqn{n_1} x \eqn{p}
#' @param y1 Outcome vector for the first sample, of length \eqn{n_1}
#' @param X2 Design matrix for the second sample, of dimension \eqn{n_2} x \eqn{p}
#' @param y2 Outcome vector for the second sample, of length \eqn{n_2}
#' @param loading Loading, of length \eqn{p}
#' @param weight The weight vector used for bias correction, of length \eqn{n}; if set to \code{NULL}, the weight is
#' the inverse of the first derivative of the logit function (default = \code{NULL})
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
#' @param init.Lasso1 Initial LASSO estimator of the first regression vector (default = \code{NULL})
#' @param init.Lasso2 Initial LASSO estimator of the second regression vector (default = \code{NULL})
#' @param lambda1 The tuning parameter in the construction of LASSO estimator of the first regression vector (default = \code{NULL})
#' @param lambda2 The tuning parameter in the construction of LASSO estimator of the second regression vector (default = \code{NULL})
#' @param mu1 The dual tuning parameter used in the construction of the first projection direction (default = \code{NULL})
#' @param mu2 The dual tuning parameter used in the construction of the second projection direction (default = \code{NULL})
#' @param step1 The step size used to compute \code{mu1}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu1}
#' such that the dual optimization problem for constructing the projection direction converges (default = \code{NULL})
#' @param step2 The step size used to compute \code{mu2}; if set to \code{NULL} it is
#' computed to be the number of steps (< \code{maxiter}) to obtain the smallest \code{mu2}
#' such that the dual optimization problem for constructing the second projection direction converges (default = \code{NULL})
#' @param resol The factor by which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu1} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 1.5)
#' @param maxiter Maximum number of steps along which \code{mu1} (and \code{mu2}) is increased/decreased to obtain the smallest \code{mu} (and \code{mu2})
#' such that the dual optimization problem for constructing the first (and the second) projection direction converges (default = 6)
#' @param alpha Level ofsignificance to test the null hypothesis which claims that the first case probability is not greater than the second case probability (default = 0.05)
#'
#' @return
#' \item{prop.est}{The bias-corrected estimator for the difference between case probabilities}
#' \item{se}{The standard error for the bias-corrected estimator}
#' \item{CI}{The confidence interval for the difference between case probabilities}
#' \item{decision}{\code{decision}\eqn{=1} implies the first case probability is greater than the second one\eqn{\newline}
#' \code{decision}\eqn{=0} implies the first case probability is less than the second one}
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit
#' @import CVXR Matrix glmnet
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
#' n1 = 100
#' n2 = 200
#' p = 400
#' mu <- rep(0,p)
#' rho = 0.5
#' Cov <- (A1gen(rho,p))/2
#' beta1 <- rep(0,p)
#' beta1[1:10] <- c(1:10)/5
#' beta2 <- rep(0,p)
#' beta2[1:5] <- c(1:5)/10
#' X1 <- MASS::mvrnorm(n1,mu,Cov)
#' X2 <- MASS::mvrnorm(n2,mu,Cov)
#' exp_val1 <- X1%*%beta1
#' exp_val2 <- X2%*%beta2
#' prob1 <- exp(exp_val1)/(1+exp(exp_val1))
#' prob2 <- exp(exp_val2)/(1+exp(exp_val2))
#' y1 <- rbinom(n1,1,prob1)
#' y2 <- rbinom(n2,1,prob2)
#' loading <- MASS::mvrnorm(1,mu,Cov)
#' Est <- ITE_Logistic(X1 = X1, y1 = y1, X2 = X2, y2 = y2,loading = loading, intercept = TRUE)
ITE_Logistic<-function(X1,y1,X2,y2,loading,weight=NULL,intercept=TRUE,init.Lasso1=NULL,init.Lasso2=NULL,lambda1=NULL,lambda2=NULL,mu1=NULL,mu2=NULL,step1=NULL,step2=NULL,resol = 1.5,maxiter=6,alpha=0.05){
  Est1<-LF_logistic(X=X1,y=y1,loading = loading,weight=weight,intercept=intercept,init.Lasso=init.Lasso1,lambda=lambda1,mu=mu1,step=step1,resol=resol,maxiter=maxiter,alpha=alpha)
  Est2<-LF_logistic(X=X2,y=y2,loading = loading,weight=weight,intercept=intercept,init.Lasso=init.Lasso2,lambda=lambda2,mu=mu2,step=step2,resol=resol,maxiter=maxiter,alpha=alpha)
  logit<-function(z)
  {
    a = log(z/(1-z))
    return(a)
  }
  debias.est<- logit(Est1$prop.est) - logit(Est2$prop.est)
  prop.est <- Est1$prop.est - Est2$prop.est
  se<-sqrt((Est1$se)^2 + (Est2$se)^2)
  CI <- c(prop.est - qnorm(1-alpha/2)*se, prop.est + qnorm(1-alpha/2)*se) ## remove if not delta
  if(debias.est - qnorm(1-alpha)*se > 0){
    dec <- 1
  }else{
    dec <- 0
  }
  returnList <- list("prop.est" = prop.est,
                     "se"=se,
                     "CI"=CI,
                     "decision" = dec
  )
  return(returnList)
}

f = function(x){
  pnorm(x)
}

fp = function(x){
  dnorm(x)
}

g = function(x){
  out = c()
  out[abs(x)<5] = (fp(x)/f(x)/(1-f(x)))[abs(x)<5]
  out[abs(x)>5] = ((abs(x)+sqrt(x^2+8/pi))/2)[abs(x)>5]
  return(out)
}

#' Inference for single regression coefficient in high dimensional probit regression model
#'
#' @description
#' Computes the bias corrected estimator of a single regression coefficinet in the high dimensional probit regression model and the corresponding standard error.
#' It also constructs the confidence interval for the concerned regression coefficient and tests whether it is equal to \code{b0} or not. Here \eqn{b0} is a believed to be the true value of the concerned regression coefficient.
#'
#' @param X Design matrix, of dimension \eqn{n} x \eqn{p}
#' @param y Outcome vector, of length \eqn{n}
#' @param loading Index (less than or equal to \eqn{p}) indicating the concerned regression coefficient. For example, \code{loading = 1} means we carry out inference for the first regression coefficient under the high-dimensional probit model.
#' @param model The high dimensional generalised linear regression model, either \code{logistic1} or \code{logistic2} or \code{probit} or \code{inverse t1} (default = \code{probit}) ; \code{model}\eqn{=}\code{"logistic1"} uses \code{SIHR::LF_logistic} with \code{weight}\eqn{=}\code{NULL}
#' @param intercept Should intercept(s) be fitted (default = \code{TRUE})
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
#' @param b0 The number indicating the true value of the concerned regression coefficient
#' @param alpha Level of significance to test the null hypothesis that the concerned regression coefficient is equal to \code{b0} (default = 0.05)
#' @param verbose Should inetrmediate message(s) be printed (default = \code{TRUE})
#'
#' @return
#' \item{prop.est}{The bias corrected estimator of the concerned regression coefficient}
#' \item{se}{The standard error of the bias-corrected estimator}
#' \item{CI}{The confidence interval for the concerned regression coefficient}
#' \item{decision}{\code{decision}\eqn{=1} implies the concerned regression coefficient is not equal to \code{b0} \eqn{\newline}
#' \code{decision}\eqn{=0} implies the concerned regression coefficient is equal to \code{b0}}
#' \item{proj}{The projection direction, of length \eqn{p}}
#' \item{plug.in}{The LASSO estimator of the concerned regression coefficient}
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats coef na.omit dnorm pnorm dt pt
#' @import CVXR Matrix glmnet
#'
#' @examples
#' sp = 20
#' n = 400
#' p = 800
#' f = function(x){
#' pnorm(x)
#' }
#' sig1 = toeplitz(seq(0.6, 0,length.out = p/10))
#' Sig = Matrix::bdiag(rep(list(sig1),10))+diag(rep(0.4,p))
#' X = MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
#' b = rep(0,p)
#' b[1:sp] = rep(c(0.4,-0.4), sp/2)
#' prob = f(X %*% b)
#' y = rep(1,n)
#' while(sum(y)/n<0.02 | sum(y)/n>0.98 ){
#'  for(gen.y in 1:n){
#'    y[gen.y] = rbinom(1,1,prob[gen.y])
#'  }
#' }
#' out.prop = SIHR::GLM_binary(X = X, y = y, loading = 1, model = "probit", intercept = FALSE)
GLM_binary<-function(X, y, loading, model = "probit", intercept = TRUE, init.Lasso = NULL, lambda = NULL, mu = NULL, step = NULL, resol = 1.5, maxiter = 6, b0 = 0, alpha = 0.05, verbose = TRUE){
  xnew <- loading
  X <- as.matrix(X)
  p <- ncol(X)
  n <- nrow(X)
  n_y <- length(y)

  if(n_y!=n)
  {
    stop("Error: Check dimensions of X and y")
  } else {
    data <- na.omit(data.frame(y,X))
    X <- as.matrix(data[,-1])
    y <- as.vector(data[,1])
    p <- ncol(X)
    n <- nrow(X)
    mean = colMeans(X)
    M = matrix(rep(mean,nrow(X)),byrow = T, nrow = nrow(X), ncol = ncol(X))
    X = X - M

    if(model == "logistic1"){
      ej = rep(0,p)
      ej[xnew] = 1
      Est <- LF_logistic(X = X, y = y, loading = ej, weight = NULL, trans = FALSE, intercept = intercept, intercept.loading = FALSE)
      return(Est)
    } else {

      if(model == "logistic2"){
        f = function(x){
          exp(x)/(1+exp(x))
        }
        fp = function(x){
          exp(x)/(1+exp(x))^2
        }
        g = function(x){
          fp(x)/f(x)/(1-f(x))
        }
      }else if(model =="probit"){
        f = function(x){
          pnorm(x)
        }
        fp = function(x){
          dnorm(x)
        }
        g = function(x){
          out=c()
          out[abs(x)<5]=(fp(x)/f(x)/(1-f(x)))[abs(x)<5]
          out[abs(x)>5]=((abs(x)+sqrt(x^2+8/pi))/2)[abs(x)>5]
          return(out)
        }
      } else if(model == "inverse t1"){
        f = function(x){
          pt(x,1)
        }
        fp = function(x){
          dt(x,1)
        }
        g = function(x){
          # out=c()
          # out[abs(x)<5]=(fp(x)/f(x)/(1-f(x)))[abs(x)<5]
          # out[abs(x)>5]=((abs(x)+sqrt(x^2+8/pi))/2)[abs(x)>5]
          # return(out)
          fp(x)/f(x)/(1-f(x))
        }
      }

    if(is.null(init.Lasso)){
      if(model == "probit"){
        fit = glmnet(X*1.75, y,  family = "binomial", alpha = 1,  intercept = intercept,
                     lambda = 0.06*sqrt(log(p)/n), standardize = FALSE)
      } else {
        if(is.null(lambda)){
          stop("Please specify a lambda! e.g., lambda = 0.1*sqrt(log(p)/n)")
        }else{
          fit = glmnet(X, y,  family = "binomial", alpha = 1,  intercept=intercept,
                       lambda = lambda, standardize=FALSE)
        }
      }
    }

    if (intercept == TRUE){
      if(is.null(init.Lasso)){
        htheta <- as.vector(coef(fit))
    } else {
        htheta <- init.Lasso
    }
    support<-(abs(htheta)>0.001)
    Xc <- cbind(rep(1,n),X);
    #col.norm <- c(1,col.norm); ###What is its use? If useful define
    pp <- (p+1);
    } else {
      if(is.null(init.Lasso)){
        htheta <- as.vector(coef(fit))[-1]
    } else {
        htheta <- init.Lasso[-1]
    }
    support<-(abs(htheta)>0.001)
    Xc <- X
    pp <- p
    }
    htheta <- as.vector(htheta)

    if(intercept==TRUE){
      loading = rep(0,pp)
      loading[xnew+1] = 1
      lasso.plugin <- htheta[xnew+1]
      exp_pred = Xc%*%(htheta)
      X.weight = diag(c(sqrt(fp(exp_pred)*g(exp_pred)))) %*% Xc
    } else {
      loading = rep(0,pp)
      loading[xnew]=1
      lasso.plugin <- htheta[xnew]
      exp_pred = Xc%*%(htheta)
      X.weight = diag(c(sqrt(fp(exp_pred)*g(exp_pred)))) %*% Xc
    }
    loading.norm = sqrt(sum(loading^2))

    count=0
    for(i in 1:ncol(X)){
      if(length(unique(X[,i])) == 1){
      count = count+1
      }
    }
    if(count!=0 && intercept==TRUE)
    {
      stop("Data is singular")
    } else {
      if ((n>=6*p)){
      sigma.hat <- (1/n)*(t(X.weight)%*%X.weight);
      tmp <- eigen(sigma.hat)
      tmp <- min(tmp$values)/max(tmp$values)
    } else {
      tmp <- 0
    }

    if ((n >= 6*p) && (tmp >= 1e-4)){
      direction <- solve(sigma.hat)%*%loading/loading.norm
    } else {
      if(is.null(step)){
        step.vec <- rep(NA,3)
        for(t in 1:3){
          index.sel <- sample(1:n,size=ceiling(0.5*min(n,p)), replace=FALSE)
          Direction.Est.temp <-  Direction_searchtuning_glm(Xc[index.sel,], loading, mu = NULL, weight = g(exp_pred)[index.sel], deriv.vec = fp(exp_pred)[index.sel], resol, maxiter)
          step.vec[t] <- Direction.Est.temp$step
        }
        step<- getmode_log(step.vec)
      }
      Direction.Est <-  Direction_fixedtuning_glm(Xc, loading, mu = sqrt(2.01*log(pp)/n)*resol^{-(step-1)}, weight = g(exp_pred), deriv.vec = fp(exp_pred))

      while(is.na(Direction.Est) || length(Direction.Est$proj)==0){
        step <- step-1
        Direction.Est <-  Direction_fixedtuning_glm(Xc, loading, mu = sqrt(2.01*log(pp)/n)*resol^{-(step-1)}, weight = g(exp_pred), deriv.vec = fp(exp_pred))
      }
      if(verbose == TRUE){
        print(paste("step is", step))
      }
      direction <- Direction.Est$proj
    }
    weighed.residual=(y - f(exp_pred))*g(exp_pred)

    correction = sum((Xc%*%direction)*weighed.residual)/n;
    debias.est=lasso.plugin+correction

    X.weight2 = diag(c(g(exp_pred)*sqrt(f(exp_pred)*(1-f(exp_pred))))) %*% Xc
    se<-sqrt(mean((X.weight2%*%direction)^2))/sqrt(n)

    CI <- c(debias.est - qnorm(1-alpha/2)*se, debias.est + qnorm(1-alpha/2)*se)

    if(debias.est - qnorm(1-alpha)*se > b0){
      dec <- 1
    } else {
      dec <- 0
    }
    returnList <- list("prop.est" = debias.est,
                       "se" = se,
                       "CI" = CI,
                       "decision" = dec,
                       "proj"=direction,
                       "step"=step,
                       "plug.in"=lasso.plugin
  )
  return(returnList)
  }
  }
}
}

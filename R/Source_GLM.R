getmode_log <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

expo <- function(z){
  g = exp(z)/(1+exp(z))
  return(g)
}

Initialization.step_log <- function(X, y, lambda = NULL, intercept = FALSE) {
  n <- nrow(X)
  col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
  Xnor <- X %*% diag(col.norm)

  htheta <- Lasso_log(Xnor, y, lambda = lambda, intercept = intercept)

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

Lasso_log <- function(X, y, lambda = NULL, intercept = TRUE) {
  p <- ncol(X)
  n <- nrow(X)

  htheta <- if (is.null(lambda)) {
    outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                        intercept = intercept)
    as.vector(coef(outLas, s = outLas$lambda.min))
  } else if (lambda == "CV") {
    outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                        intercept = intercept)
    as.vector(coef(outLas, s = outLas$lambda.1se))
  } else {
    outLas <- glmnet(X, y, family = "binomial", alpha = 1,
                     intercept = intercept)
    as.vector(coef(outLas, s = lambda))
  }
  if (intercept == TRUE) {
    return(htheta)
  } else {
    return(htheta[2:(p+1)])
  }
}

Direction_fixedtuning_glm <- function(X, loading, mu = NULL, weight, deriv.vec){
  pp <- ncol(X)
  n <- nrow(X)
  if(is.null(mu)){
    mu <- sqrt(2.01*log(pp)/n)
  }
  loading.norm <- sqrt(sum(loading^2))
  if (loading.norm == 0){
    H <- cbind(loading, diag(1, pp))
  }else{
    H <- cbind(loading / loading.norm, diag(1, pp))
  }
  v<-Variable(pp+1)
  obj <- 1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
  prob <- Problem(Minimize(obj))
  result <- solve(prob)
  if(result$status=="optimal" || result$status == "unbounded"){
    opt.sol<-result$getValue(v)
    cvxr_status<-result$status
    direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  }else{
    direction <- numeric(0)
  }
  returnList <- list("proj"=direction)
  return(returnList)
}

Direction_searchtuning_glm <- function(X,loading,mu=NULL,weight,deriv.vec,resol=1.5, maxiter=6){
  pp <- ncol(X)
  n <- nrow(X)
  tryno <- 1
  opt.sol <- rep(0,pp+1)
  lamstop <- 0
  cvxr_status <- "optimal"
  mu <- sqrt(2.01*log(pp)/n)
  while (lamstop == 0 && tryno < maxiter){
    lastv <- opt.sol
    lastresp <- cvxr_status
    loading.norm <- sqrt(sum(loading^2))
    if (loading.norm == 0){
      H <- cbind(loading, diag(1, pp))
    }else{
      H <- cbind(loading / loading.norm, diag(1, pp))
    }
    v <- Variable(pp+1)
    obj <- 1/4*sum(((X%*%H%*%v)^2)*weight*deriv.vec)/n+sum((loading/loading.norm)*(H%*%v))+mu*sum(abs(v))
    prob <- Problem(Minimize(obj))
    result <- solve(prob)
    cvxr_status <- result$status
    if(tryno == 1){
      if(cvxr_status == "optimal"){
        incr <- 0
        mu <- mu/resol
        opt.sol <- result$getValue(v)
        temp.vec <- (-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
        initial.sd <- sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm
        temp.sd <- initial.sd
      }else{
        incr <- 1
        mu <- mu*resol
      }
    }else{
      if(incr == 1){
        if(cvxr_status == "optimal"){
          lamstop <- 1
          opt.sol <- result$getValue(v)
        }else{
          mu <- mu*resol
        }
      }else{
        if(cvxr_status == "optimal" && temp.sd < 3*initial.sd){
          mu <- mu/resol
          opt.sol <- result$getValue(v)
          temp.vec <- (-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
          temp.sd <- sqrt(sum(((X%*% temp.vec)^2)*weight*deriv.vec)/(n)^2)*loading.norm
        }else{
          mu <- mu*resol
          opt.sol <- lastv
          lamstop <- 1
          tryno <- tryno-1
        }
      }
    }
    tryno <- tryno + 1
  }
  direction <- (-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  step <- tryno-1
  returnList <- list("proj"=direction,
                     "step"=step)
  return(returnList)
}

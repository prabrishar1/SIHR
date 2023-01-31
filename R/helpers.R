getmode <- function(v) {
  tbl <- table(v)
  if (all(tbl == 1)) {
    median(v)
  } else {
    as.numeric(names(which.max(tbl)))
  }
}

relevant.funs <- function(intercept=TRUE, model=c("linear","logistic","logistic_alter")){
  model = match.arg(model)

  ### init step function ###
  if(model=="linear"){

    train.fun <- function(X, y,lambda=NULL){
      if(is.null(lambda)) lambda = "CV.min"
      p = ncol(X)
      htheta <- if (lambda == "CV.min") {
        outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                            intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = outLas$lambda.min))
      } else if (lambda == "CV") {
        outLas <- cv.glmnet(X, y, family = "gaussian", alpha = 1,
                            intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = outLas$lambda.1se))
      } else {
        outLas <- glmnet(X, y, family = "gaussian", alpha = 1,
                         intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = lambda))
      }
      if(intercept==FALSE) htheta = htheta[2:(p+1)]

      return(list(lasso.est = htheta))
    }

    cond_var.fun <- function(pred, y=NULL, sparsity=NULL){
      n = length(y)
      sigma.sq = sum((y - pred)^2) / max(0.7*n, n-sparsity)
      return(rep(sigma.sq, n))
    }

  }else{

    train.fun <- function(X, y, lambda=NULL){
      if(is.null(lambda)) lambda = "CV.min"
      p = ncol(X)
      htheta <- if (lambda == "CV.min") {
        outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                            intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = outLas$lambda.min))
      } else if (lambda == "CV") {
        outLas <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                            intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = outLas$lambda.1se))
      } else {
        outLas <- glmnet(X, y, family = "binomial", alpha = 1,
                         intercept = intercept, standardize = T)
        as.vector(coef(outLas, s = lambda))
      }
      if(intercept==FALSE) htheta = htheta[2:(p+1)]

      return(list(lasso.est = htheta))
    }

    cond_var.fun <- function(pred, y=NULL, sparsity=NULL){
      cond_var = pred * (1 - pred)
      return(cond_var)
    }
  }

  if(model=="linear"){
    pred.fun = function(x) x
    deriv.fun = function(x) rep(1, length(x))
    weight.fun = function(x) rep(1, length(x))
  }else if(model=="logistic"){
    pred.fun = function(x) exp(x)/(1+exp(x))
    deriv.fun = function(x) exp(x)/(1+exp(x))^2
    weight.fun = function(x) (1+exp(x))^2/exp(x)
  }else if(model=="logistic_alter"){
    pred.fun = function(x) exp(x)/(1+exp(x))
    deriv.fun = function(x) exp(x)/(1+exp(x))^2
    weight.fun = function(x) rep(1, length(x))
  }
  # else if(model=="probit"){
  #   pred.fun = function(x) pnorm(x)
  #   deriv.fun = function(x) dnorm(x)
  #   weight.fun = function(x){
  #     out = c()
  #     out[abs(x)<5]=(deriv.fun(x)/pred.fun(x)/(1-pred.fun(x)))[abs(x)<5]
  #     out[abs(x)>5]=((abs(x)+sqrt(x^2+8/pi))/2)[abs(x)>5]
  #     return(out)
  #   }
  # }

  return(list(train.fun    = train.fun,
              pred.fun     = pred.fun,
              deriv.fun    = deriv.fun,
              weight.fun   = weight.fun,
              cond_var.fun = cond_var.fun))
}

stars.pval <- function(p.value){
  unclass(symnum(p.value, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", ".", " ")))
}

Direction_searchtuning <- function(X, loading, weight, deriv, resol=1.5, maxiter=10){
  p = ncol(X)
  n = nrow(X)
  mu = sqrt(2.01*log(p)/n)
  opt.sol = rep(0, p+1)
  loading.norm = sqrt(sum(loading^2))
  H = cbind(loading/loading.norm, diag(1, p))

  ## 1st iteration to decide whether increase mu or decrease mu
  iter = 1
  v = Variable(p+1)
  adj.XH = sqrt(weight)*sqrt(deriv)*(X%*%H)
  obj = 1/4*sum_squares(adj.XH%*%v)/n + sum((loading/loading.norm)*(H%*%v)) + mu*sum(abs(v))
  # obj = 1/4*sum(((X%*%H%*%v)^2)*weight*deriv)/n + sum((loading/loading.norm)*(H%*%v)) + mu*sum(abs(v))
  prob = Problem(Minimize(obj))
  result = solve(prob)
  status = result$status
  if(status=="optimal"){
    incr = -1
    v_opt = result$getValue(v)
  }else{
    incr = 1
  }

  ## while loop to find the best mu (the smallest mu satisfying optimal status)
  while(iter <= maxiter){
    laststatus = status
    mu = mu*(resol^incr)
    obj = 1/4*sum_squares(adj.XH%*%v)/n + sum((loading/loading.norm)*(H%*%v)) + mu*sum(abs(v))
    prob = Problem(Minimize(obj))
    result = solve(prob)
    status = result$status
    if(incr==-1){
      if(status=='optimal'){
        v_opt = result$getValue(v)
        iter = iter+1
        next
      }else{
        step = iter - 1
        break
      }
    }
    if(incr==1){
      if(status!='optimal'){
        iter = iter+1
        next
      }else{
        step = iter
        v_opt = result$getValue(v)
        break
      }
    }
  }
  if(iter > maxiter) step = maxiter

  direction = -(1/2)*(v_opt[-1] + v_opt[1]*loading/loading.norm)
  return(list(proj = direction,
              step = step,
              incr = incr,
              laststatus = laststatus,
              curstatus = status,
              mu = mu))
}

Direction_fixedtuning <- function(X, loading, weight, deriv, mu=NULL, resol=1.5, step=3, incr=-1){
  p <- ncol(X)
  n <- nrow(X)
  if(is.null(mu)){
    mu = sqrt(2.01*log(p)/n)
    mu = mu * resol^{incr*step}
  }
  loading.norm <- sqrt(sum(loading^2))

  H <- cbind(loading / loading.norm, diag(1, p))
  v <- Variable(p+1)
  adj.XH = sqrt(weight)*sqrt(deriv)*(X%*%H)
  obj <- 1/4*sum_squares(adj.XH%*%v)/n + sum((loading/loading.norm)*(H%*%v)) + mu*sum(abs(v))
  prob <- Problem(Minimize(obj))
  result <- solve(prob)
  opt.sol<-result$getValue(v)
  status<-result$status
  direction<-(-1)/2*(opt.sol[-1]+opt.sol[1]*loading/loading.norm)
  return(list(proj=direction,
              status = status,
              mu=mu))
}

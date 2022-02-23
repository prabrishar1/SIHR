getmode <- function(v) {
  tbl <- table(v)
  if (all(tbl == 1)) {
    median(v)
  } else {
    as.numeric(names(which.max(tbl)))
  }
}
#getmode_log <- function(v) {
#  uniqv <- unique(v)
#  uniqv[which.max(tabulate(match(v, uniqv)))]
#}
Initialization.step <- function(X, y, model = "linear", lambda = NULL, intercept = FALSE) {
  n <- nrow(X)
  col.norm <- 1 / sqrt((1 / n) * diag(t(X) %*% X))
  Xnor <- X %*% diag(col.norm)

  htheta <- Lasso(Xnor, y, model = model, lambda = lambda, intercept = intercept)

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

Lasso<- function(X, y, model = "linear", lambda = NULL, intercept = TRUE) {
  p <- ncol(X)
  n <- nrow(X)
  if(model == "linear"){
    family = "gaussian"
    standardize = TRUE
  } else if(model == "glm"){
    family = "binomial"
    standardize = FALSE
  }

  htheta <- if (is.null(lambda)) {
    outLas <- cv.glmnet(X, y, family = family, alpha = 1,
                        intercept = intercept, standardize = standardize)
    as.vector(coef(outLas, s = outLas$lambda.min))
  } else if (lambda == "CV") {
    outLas <- cv.glmnet(X, y, family = family, alpha = 1,
                        intercept = intercept, standardize = standardize)
    as.vector(coef(outLas, s = outLas$lambda.1se))
  } else if (lambda == "scalreg" && model == "linear") {
    Xc <- if (intercept) {
      cbind(rep(1, n), X)
    } else {
      X
    }
    outLas <- scalreg(Xc, y)
    if (intercept) {
      outLas$coefficients
    } else {
      c(0, outLas$coefficients)
    }
  }else {
    outLas <- glmnet(X, y, family = family, alpha = 1,
                     intercept = intercept, standardize = standardize)
    as.vector(coef(outLas, s = lambda))
  }
  if (intercept == TRUE) {
    return(htheta)
  } else {
    return(htheta[2:(p+1)])
  }
}

#' Summarizing LF
#' @description `summary` method for class `LF`
#' @param obj An object of class `LF`, a result of a call to `LF`
#' @param alpha Level of significance to construct confidence interval
#' @param alternative indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param digits The number of digits to use when printing
#' @return The function `summary.LF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' \item{output.ci}{a \eqn{ncol(loading.mat)} x 3 matrix with columns for the loading, lower and upper bounds
#' of (two-sided) confidence intervals; Each row corresponds to each loading.}
#' @export
#'
#' @examples
#' \dontrun{
#' ##-- Continuing the LF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.LF <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater"), digits=4){
  alternative = match.arg(alternative)
  est.plugin.vec = obj$est.plugin.vec
  est.debias.vec = obj$est.debias.vec
  se.vec         = obj$se.vec

  n.loading = length(se.vec)
  output.est = data.frame(matrix(NA, nrow=n.loading, ncol=7))
  colnames(output.est) = c("loading","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = 1:n.loading
  output.est[,c(2,3,4)] = cbind(est.plugin.vec, est.debias.vec, se.vec)
  output.est[,5] = est.debias.vec / se.vec
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  obj = list(output.est = output.est,
             output.ci  = output.ci,
             digits = digits)
  class(obj) = "summary.LF"
  obj
}

#' Summarizing ITE
#' @description `summary` method for class `ITE`
#' @param obj An object of class `ITE`, a result of a call to `ITE`
#' @param alpha Level of significance to construct confidence interval
#' @param alternative indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param digits The number of digits to use when printing
#' @return The function `summary.ITE` computes and returns a list of summary statistics
#' of ITE given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' \item{output.ci}{a \eqn{ncol(loading.mat)} x 3 matrix with columns for the loading, lower and upper bounds
#' of (two-sided) confidence intervals; Each row corresponds to each loading.}
#' @export
#'
#' @examples
#' \dontrun{
#' ##-- Continuing the ITE(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.ITE <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater"), digits=4){
  alternative = match.arg(alternative)
  est.plugin.vec = obj$est.plugin.vec
  est.debias.vec = obj$est.debias.vec
  se.vec         = obj$se.vec

  n.loading = length(se.vec)
  output.est = data.frame(matrix(NA, nrow=n.loading, ncol=7))
  colnames(output.est) = c("loading","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = 1:n.loading
  output.est[,c(2,3,4)] = cbind(est.plugin.vec, est.debias.vec, se.vec)
  output.est[,5] = est.debias.vec / se.vec
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  obj = list(output.est = output.est,
             output.ci  = output.ci,
             digits = digits)
  class(obj) = "summary.ITE"
  obj
}

#' Summarizing QF
#' @description `summary` method for class `QF`
#' @param obj An object of class `QF`, a result of a call to `QF`
#' @param alpha Level of significance to construct confidence interval
#' @param alternative indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param digits The number of digits to use when printing
#'
#' @return The function `summary.QF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{length(tau.vec)} x 7 matrix with columns for tau,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each tau.}
#' \item{output.ci}{a \eqn{length(tau.vec)} x 3 matrix with columns for tau, lower and upper bounds
#' of (two-sided) confidence intervals; Each row corresponds to each tau.}
#' @export
#'
#' @examples
#' \dontrun{
#' ##-- Continuing the QF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.QF <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater"), digits=4){
  alternative = match.arg(alternative)
  est.plugin = obj$est.plugin
  est.debias = obj$est.debias
  se.vec     = obj$se.vec
  tau.vec    = obj$tau.vec

  n.tau = length(tau.vec)
  output.est = data.frame(matrix(NA, nrow=n.tau, ncol=7))
  colnames(output.est) = c("tau","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = tau.vec
  output.est[,c(2,3,4)] = cbind(rep(est.plugin, n.tau), rep(est.debias, n.tau), se.vec)
  output.est[,5] = rep(est.debias, n.tau) / se.vec
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(tau.vec, output.ci))
  colnames(output.ci) = c("tau","lower","upper")
  rownames(output.ci) = paste("tau",tau.vec, sep="")
  obj = list(output.est = output.est,
             output.ci  = output.ci,
             digits = digits)
  class(obj) = "summary.QF"
  obj
}


#' Summarizing LF
#' @description `summary` method for class `LF`
#' @param obj An object of class `summary.LF`, a result of a call to `summary.LF`
#' @return The function `summary.LF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 5 matrix with columns for the
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic and
#' corresponding (two-sided) p-value; Each row corresponds to each loading.}
#' \item{output.ci}{a \eqn{ncol(loading.mat)} x 2 matrix with columns for lower and upper
#' of (two-sided) confidence intervals; Each row corresponds to each loading.}
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the LF(.) example:
#' summary(Est)
#' }
print.summary.LF <- function(obj){
  output.est = obj$output.est
  output.ci  = obj$output.ci
  digits = obj$digi

  cat("Call: \nInference for Linear Functional\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  # print(noquote(format(output.est, digits=digits)))
  cat("\nConfidence Intervals:\n")
  print(output.ci, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

#' Summarizing ITE
#' @description `summary` method for class `ITE`
#' @param obj An object of class `summary.ITE`, a result of a call to `summary.ITE`
#' @return The function `summary.ITE` computes and returns a list of summary statistics
#' of ITE given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 5 matrix with columns for the
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic and
#' corresponding (two-sided) p-value; Each row corresponds to each loading.}
#' \item{output.ci}{a \eqn{ncol(loading.mat)} x 2 matrix with columns for lower and upper
#' of (two-sided) confidence intervals; Each row corresponds to each loading.}
#' @export
#'
#' @examples
#' \dontrun{
#' #' ##-- Continuing the ITE(.) example:
#' summary(Est)
#' }
print.summary.ITE <- function(obj){
  output.est = obj$output.est
  output.ci  = obj$output.ci

  cat("Call: \nInference for Treatment Effect\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  # print(noquote(format(output.est, digits=digits)))
  cat("\nConfidence Intervals:\n")
  print(output.ci, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

#' Summarizing QF
#' @description `summary` method for class `QF`
#' @param obj An object of class `summary.QF`, a result of a call to `summary.QF`
#' @return The function `summary.QF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{length(tau.vec)} x 5 matrix with columns for the
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic and
#' corresponding (two-sided) p-value; Each row corresponds to each tau.}
#' \item{output.ci}{a \eqn{length(tau.vc)} x 2 matrix with columns for lower and upper
#' of (two-sided) confidence intervals; Each row corresponds to each tau.}
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the QF(.) example:
#' summary(Est)
#' }
print.summary.QF <- function(obj){
  output.est = obj$output.est
  output.ci  = obj$output.ci

  cat("Call: \nInference for Quadratic Functional\n\n")
  print(output.est, digits=digits, quote=F, row.names=F)
  # print(noquote(format(output.est, digits=digits)))
  cat("\nConfidence Intervals:\n")
  print(output.ci, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

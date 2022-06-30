############################################
########## Methods for class LF ############
############################################

#' Confidence Intervals for Bias-corrected LF Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a loading.
#' @param obj An object of class `LF`, a result of a call to `LF`
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#'
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators.
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the LF(.) example:
#' ci = confint(Est)
#' ci
#' }
confint.LF <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater")){
  alternative = match.arg(alternative)
  est.debias.vec = obj$est.debias.vec
  se.vec         = obj$se.vec
  n.loading = length(se.vec)
  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  return(output.ci)
}

#' Summarizing LF
#' @description `summary` method for class `LF`
#' @param obj An object of class `LF`, a result of a call to `LF`
#' @return The function `summary.LF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the LF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.LF <- function(obj){
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

  obj = list(output.est = output.est)
  class(obj) = "summary.LF"
  obj
}

#' Printing Summarizing LF
#' @description `print` method for class `summary.LF`
#' @param obj An object of class `summary.LF`, a result of a call to `summary.LF`
#' @param digits The number of digits to use when printing (default=4)
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the LF(.) example:
#' summary(Est)
#' }
print.summary.LF <- function(obj, digits=4){
  output.est = obj$output.est
  cat("Call: \nInference for Linear Functional\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

#############################################
########## Methods for class ITE ############
#############################################

#' Confidence Intervals for Bias-corrected ITE Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a loading.
#' @param obj An object of class `ITE`, a result of a call to `ITE`
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators.
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the ITE(.) example:
#' ci = confint(Est)
#' ci
#' }
confint.ITE <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater")){
  alternative = match.arg(alternative)
  est.debias.vec = obj$est.debias.vec
  se.vec         = obj$se.vec
  n.loading = length(se.vec)
  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  return(output.ci)
}

#' Summarizing ITE
#' @description `summary` method for class `ITE`
#' @param obj An object of class `ITE`, a result of a call to `ITE`
#' @return The function `summary.ITE` computes and returns a list of summary statistics
#' of ITE given `obj`
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the ITE(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.ITE <- function(obj){
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

  obj = list(output.est = output.est)
  class(obj) = "summary.ITE"
  obj
}

#' Printing Summarizing ITE
#' @description `print` method for class `summary.ITE`
#' @param obj An object of class `summary.ITE`, a result of a call to `summary.ITE`
#' @param digits The number of digits to use when printing (default=4)
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the ITE(.) example:
#' summary(Est)
#' }
print.summary.ITE <- function(obj, digits=4){
  output.est = obj$output.est
  cat("Call: \nInference for Treatment Effect\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

############################################
########## Methods for class QF ############
############################################

#' Confidence Intervals for Bias-corrected QF Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a tau value.
#' @param obj An object of class `QF`, a result of a call to `QF`
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators, with rows corresponding to different tau.
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the QF(.) example:
#' ci = confint(Est)
#' ci
#' }
confint.QF <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater")){
  alternative = match.arg(alternative)
  est.debias = obj$est.debias
  se.vec     = obj$se.vec
  tau.vec    = obj$tau.vec
  n.tau = length(tau.vec)
  if(alternative=="two.sided"){
    output.ci = cbind(est.debias - qnorm(1-alpha/2)*se.vec, est.debias + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias + qnorm(1-alpha)*se)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias - qnorm(1-alpha)*se, Inf)
  }
  output.ci = data.frame(cbind(tau.vec, output.ci))
  colnames(output.ci) = c("tau","lower","upper")
  return(output.ci)
}

#' Summarizing QF
#' @description `summary` method for class `QF`
#' @param obj An object of class `QF`, a result of a call to `QF`
#' @return The function `summary.QF` computes and returns a list of summary statistics
#' of LF given `obj`
#' \item{output.est}{a \eqn{length(tau.vec)} x 7 matrix with columns for tau,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each tau.}
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the QF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.QF <- function(obj, alpha=0.05, alternative=c("two.sided","less","greater"), digits=4){
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

  obj = list(output.est = output.est)
  class(obj) = "summary.QF"
  obj
}

#' Printing ummarizing QF
#' @description `print` method for class `summary.QF`
#' @param obj An object of class `summary.QF`, a result of a call to `summary.QF`
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the QF(.) example:
#' summary(Est)
#' }
print.summary.QF <- function(obj){
  output.est = obj$output.est

  cat("Call: \nInference for Quadratic Functional\n\n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(obj)
}

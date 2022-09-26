#' Confidence Intervals for Bias-corrected Estimators
#' @description generic function
#' @param object An object of class
#' @param probability Whether returns CI with probability transformation or not (default=\code{FALSE})
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param ... arguments to pass down
#' @keywords internal
#' @export
ci <- function(object, probability=FALSE, alpha=0.05, alternative=c("two.sided","less","greater"), ...){
  UseMethod("ci")
}
############################################
########## Methods for class LF ############
############################################

#' Confidence Intervals for Bias-corrected LF Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a loading.
#' @param object An object of class `LF`, a result of a call to `LF`
#' @param probability Whether returns CI with probability transformation or not (default=\code{FALSE})
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param ... arguments to pass down
#' @keywords internal
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators.
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the LF(.) example:
#' out = ci(Est)
#' out
#' }
ci.LF <- function(object, probability=FALSE, alpha=0.05, alternative=c("two.sided","less","greater"), ...){
  alternative = match.arg(alternative)
  est.debias.vec = object$est.debias.vec
  se.vec         = object$se.vec
  n.loading = length(se.vec)
  if(alternative=="two.sided"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
  }else if(alternative=="less"){
    output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se.vec)
  }else if(alternative=="greater"){
    output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se.vec, Inf)
  }
  if(probability) output.ci = exp(output.ci) / (1 + exp(output.ci))
  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  return(output.ci)
}

#' Summarizing LF
#' @description `summary` method for class `LF`
#' @param object An object of class `LF`, a result of a call to `LF`
#' @param ... arguments to pass down
#' @return The function `summary.LF` computes and returns a list of summary statistics.
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the LF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.LF <- function(object, ...){
  est.plugin.vec = object$est.plugin.vec
  est.debias.vec = object$est.debias.vec
  se.vec         = object$se.vec
  n.loading = length(se.vec)
  output.est = data.frame(matrix(NA, nrow=n.loading, ncol=7))
  colnames(output.est) = c("loading","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = 1:n.loading
  output.est[,c(2,3,4)] = cbind(est.plugin.vec, est.debias.vec, se.vec)
  output.est[,5] = est.debias.vec / se.vec
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  x = list(output.est = output.est)
  class(x) = "summary.LF"
  x
}

#' Printing Summarizing LF
#' @description `print` method for class `summary.LF`
#' @param x An object of class `summary.LF`, a result of a call to `summary.LF`
#' @param digits The number of digits to use when printing
#' @param ... arguments to pass down
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the LF(.) example:
#' summary(Est)
#' }
print.summary.LF <- function(x, digits = max(3, getOption("digits") - 3), ...){
  output.est = x$output.est
  cat("Call: \nInference for Linear Functional\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(x)
}

#############################################
########## Methods for class ITE ############
#############################################

#' Confidence Intervals for Bias-corrected ITE Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a loading.
#' @param object An object of class `ITE`, a result of a call to `ITE`
#' @param probability Whether returns CI with probability transformation or not (default=\code{FALSE})
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param ... arguments to pass down
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the ITE(.) example:
#' out = ci(Est)
#' out
#' }
ci.ITE <- function(object, probability=FALSE, alpha=0.05, alternative=c("two.sided","less","greater"), ...){
  alternative = match.arg(alternative)

  if(probability==FALSE){
    est.debias.vec = object$est.debias.vec
    se.vec         = object$se.vec
    n.loading = length(se.vec)
    if(alternative=="two.sided"){
      output.ci = cbind(est.debias.vec - qnorm(1-alpha/2)*se.vec, est.debias.vec + qnorm(1-alpha/2)*se.vec)
    }else if(alternative=="less"){
      output.ci = cbind(-Inf, est.debias.vec + qnorm(1-alpha)*se.vec)
    }else if(alternative=="greater"){
      output.ci = cbind(est.debias.vec - qnorm(1-alpha)*se.vec, Inf)
    }
  }else{
    prob.debias.vec = object$prob.debias.vec
    prob.se.vec = object$prob.se.vec
    n.loading = length(prob.se.vec)
    if(n.loading == 0){
      stop("For linear model, argument 'probability' must be FALSE\n")
    }
    if(alternative=="two.sided"){
      output.ci = cbind(prob.debias.vec - qnorm(1-alpha/2)*prob.se.vec,
                        prob.debias.vec + qnorm(1-alpha/2)*prob.se.vec)
    }else if(alternative=="less"){
      output.ci = cbind(-1, prob.debias.vec + qnorm(1-alpha)*prob.se.vec)
    }else if(alternative=="greater"){
      output.ci = cbind(prob.debias.vec - qnorm(1-alpha)*prob.se.vec, 1)
    }
  }

  output.ci = data.frame(cbind(1:n.loading, output.ci))
  colnames(output.ci) = c("loading","lower","upper")
  return(output.ci)
}

#' Summarizing ITE
#' @description `summary` method for class `ITE`
#' @param object An object of class `ITE`, a result of a call to `ITE`
#' @param ... arguments to pass down
#' @return The function `summary.ITE` computes and returns a list of summary statistics.
#' \item{output.est}{a \eqn{ncol(loading.mat)} x 7 matrix with columns for the loading,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each loading.}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the ITE(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.ITE <- function(object, ...){
  est.plugin.vec = object$est.plugin.vec
  est.debias.vec = object$est.debias.vec
  se.vec         = object$se.vec
  n.loading = length(se.vec)
  output.est = data.frame(matrix(NA, nrow=n.loading, ncol=7))
  colnames(output.est) = c("loading","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = 1:n.loading
  output.est[,c(2,3,4)] = cbind(est.plugin.vec, est.debias.vec, se.vec)
  output.est[,5] = est.debias.vec / se.vec
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  x = list(output.est = output.est)
  class(x) = "summary.ITE"
  x
}

#' Printing Summarizing ITE
#' @description `print` method for class `summary.ITE`
#' @param x An object of class `summary.ITE`, a result of a call to `summary.ITE`
#' @param digits The number of digits to use when printing
#' @param ... arguments to pass down
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the ITE(.) example:
#' summary(Est)
#' }
print.summary.ITE <- function(x, digits = max(3, getOption("digits") - 3), ...){
  output.est = x$output.est
  cat("Call: \nInference for Treatment Effect\n\n")
  cat("Estimators: \n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(x)
}

############################################
########## Methods for class QF ############
############################################

#' Confidence Intervals for Bias-corrected QF Estimators
#' @description Computes confidence intervals for bias-corrected estimators; Each
#' row corresponds to a tau value.
#' @param object An object of class `QF`, a result of a call to `QF`
#' @param probability Whether returns CI with probability transformation or not (default=\code{FALSE})
#' @param alpha Level of significance to construct confidence interval (default=0.05)
#' @param alternative Indicates the alternative hypothesis to construct confidence interval and must be one of "two.sided" (default), "less", or "greater".
#' @param ... arguments to pass down
#' @return A matrix with columns giving lower and upper confidence limits for bias-corrected
#' estimators, with rows corresponding to different tau.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the QF(.) example:
#' out = ci(Est)
#' out
#' }
ci.QF <- function(object, probability=FALSE, alpha=0.05, alternative=c("two.sided","less","greater"), ...){
  if(probability==TRUE){
    cat("QF only supports probability=FALSE \n")
    probability=FALSE
  }

  alternative = match.arg(alternative)
  est.debias = object$est.debias
  se     = object$se
  tau    = object$tau
  n.tau = length(tau)
  if(alternative=="two.sided"){
    output.ci = cbind(pmax(est.debias - qnorm(1-alpha/2)*se,0), pmax(est.debias + qnorm(1-alpha/2)*se,0))
  }else if(alternative=="less"){
    output.ci = cbind(0, pmax(est.debias + qnorm(1-alpha)*se,0))
  }else if(alternative=="greater"){
    output.ci = cbind(pmax(est.debias - qnorm(1-alpha)*se,0), Inf)
  }
  output.ci = data.frame(cbind(tau, output.ci))
  colnames(output.ci) = c("tau","lower","upper")
  return(output.ci)
}

#' Summarizing QF
#' @description `summary` method for class `QF`
#' @param object An object of class `QF`, a result of a call to `QF`
#' @param ... arguments to pass down
#' @return The function `summary.QF` computes and returns a list of summary statistics.
#' \item{output.est}{a \eqn{length(tau.vec)} x 7 matrix with columns for tau,
#' plugin(biased) estimators, bias-corrected estimators, its standard error, z-statistic,
#' corresponding (two-sided) p-value and significance stars; Each row corresponds to each tau.}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' ##-- Continuing the QF(.) example:
#' sEst = summary(Est)
#' sEst
#' }
summary.QF <- function(object, ...){
  est.plugin = object$est.plugin
  est.debias = object$est.debias
  se = object$se
  tau = object$tau

  n.tau = length(tau)
  output.est = data.frame(matrix(NA, nrow=n.tau, ncol=7))
  colnames(output.est) = c("tau","est.plugin","est.debias","Std. Error","z value","Pr(>|z|)", "")
  output.est[,1] = tau
  output.est[,c(2,3,4)] = cbind(rep(est.plugin, n.tau), rep(est.debias, n.tau), se)
  output.est[,5] = rep(est.debias, n.tau) / se
  output.est[,6] = apply(cbind(pnorm(output.est[,5]), 1-pnorm(output.est[,5])), MARGIN = 1, FUN=min)*2
  output.est[,7] = stars.pval(output.est[,6])

  x = list(output.est = output.est)
  class(x) = "summary.QF"
  x
}

#' Printing ummarizing QF
#' @description `print` method for class `summary.QF`
#' @param x An object of class `summary.QF`, a result of a call to `summary.QF`
#' @param digits The number of digits to use when printing
#' @param ... arguments to pass down
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #' ##-- Continuing the QF(.) example:
#' summary(Est)
#' }
print.summary.QF <- function(x, digits = max(3, getOption("digits") - 3), ...){
  output.est = x$output.est

  cat("Call: \nInference for Quadratic Functional\n\n")
  print(output.est, digits=digits, quote=F, row.names=F)
  invisible(x)
}

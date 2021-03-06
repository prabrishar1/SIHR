% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Projection_Directions.R
\name{Direction_searchtuning}
\alias{Direction_searchtuning}
\title{Searches for the best step size and computes the projection direction with the searched best step size}
\usage{
Direction_searchtuning(
  X,
  loading,
  model = "linear",
  weight = NULL,
  deriv.vec = NULL,
  resol = 1.5,
  maxiter = 6
)
}
\arguments{
\item{X}{Design matrix, of dimension \eqn{n} x \eqn{p}}

\item{loading}{Loading, of length \eqn{p}}

\item{model}{The high dimensional regression model, either \code{linear} or \code{logistic} (default = \code{linear})}

\item{weight}{The weight vector of length \eqn{n}; to be supplied if \code{model="logistic"} (default=\code{NULL} when \code{model=linear})}

\item{deriv.vec}{The first derivative vector of the logit function at \eqn{X\%*\%}(\code{init.Lasso}), of length \eqn{n} ; to be supplied if \code{model="logistic"}. Here \code{init.Lasso} is the initial Lasso estimate of the regression vector. (default = \code{NULL} when \code{model=linear})}

\item{resol}{The factor by which \code{mu} is increased/decreased to obtain the smallest \code{mu}
such that the dual optimization problem for constructing the projection direction converges (default = 1.5)}

\item{maxiter}{Maximum number of steps along which \code{mu} is increased/decreased to obtain the smallest \code{mu}
such that the dual optimization problem for constructing the projection direction converges (default = 6)}
}
\value{
\item{proj}{The projection direction, of length \eqn{p}}
\item{step}{The best step size}
}
\description{
Searches for the best step size and computes the projection direction with the searched best step size
}
\examples{
n = 100
p = 400
X = matrix(sample(-2:2,n*p,replace = TRUE),nrow = n,ncol = p)
Direction_searchtuning(X,loading=c(1,rep(0,(p-1))))
}

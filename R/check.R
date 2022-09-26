check.args.LF <- function(X=NULL, y=NULL, loading.mat=NULL, model=NULL, intercept=NULL, intercept.loading=NULL,
                          beta.init=NULL, lambda=NULL, mu=NULL, prob.filter=NULL, rescale=NULL, alpha=NULL, verbose=NULL){
  if(is.null(X) || !is.numeric(X)) stop("X must be a numeric matrix")
  if(is.null(y) || !is.numeric(y)) stop("y must be a numeric vector")
  if(nrow(X) != length(y)) stop("nrow(X) and length(y) must match")
  if(is.null(loading.mat) || !is.numeric(loading.mat)) stop("loading must be a numeric matrix")
  if(ncol(X) != nrow(loading.mat)) stop("ncol(X) and nrow(loading) must match")
  if(is.null(model) || !model%in%c("linear","logistic","logistic_alter","probit")){
    stop("specified model has not been developed yet")
  }
  if(is.null(intercept) || length(intercept)!=1 || !is.logical(intercept)){
    stop("intercept must be a Boolean")
  }
  if(is.null(intercept.loading) || length(intercept.loading)!=1 || !is.logical(intercept)){
    stop("intercept must be a Boolean")
  }
  if(!is.null(beta.init)){
    if(!is.numeric(beta.init)) stop("beta.init must be numeric")
    if(is.vector(beta.init)){
      if(intercept) if(length(beta.init)!=(ncol(X)+1)) stop("intercept is TRUE, beta.init must be length of ncol(X)+1")
      if(!intercept) if(length(beta.init)!=ncol(X)) stop("intercept is FALSE, beta.init must be length of ncol(X)")
    }
    if(is.matrix(beta.init)){
      if(ncol(beta.init)!=1) stop("If beta.init is a matrix, ncol(beta.init) must be 1")
      if(intercept) if(nrow(beta.init)!=(ncol(X)+1)) stop("intercept is TRUE, beta.init must be length of ncol(X)+1")
      if(!intercept) if(nrow(beta.init)!=ncol(X)) stop("intercept is FALSE, beta.init must be length of ncol(X)")
    }
  }
  if(!is.null(lambda)){
    if(length(lambda)!=1) stop("lambda must be length of 1")
    if(!is.numeric(lambda)) if(!(lambda %in% c("CV.min","CV"))) stop("lambda must be a number, except from 'CV.min','CV'.")
  }
  if(!is.null(mu)){
    if(length(mu)!=1) stop("mu must be length of 1")
    if(!is.numeric(mu)) stop("mu must be number")
    if(mu<=0) stop("mu must be positive")
  }
  if(is.null(rescale) || !is.numeric(rescale) || length(rescale)!=1) stop("rescale must be numeric")
  if(is.null(prob.filter) || !is.numeric(prob.filter) || length(prob.filter)!=1 || prob.filter < 0 || prob.filter > 0.5) stop("prob.filter must be numeric in (0,0.5)")
  if(is.null(alpha) || length(alpha)!= 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1){
    stop("alpha must be a number between 0 and 1")
  }
  if(is.null(verbose) || !is.logical(verbose) || length(verbose)!=1 ) stop("verbose must be a Boolean")
}

check.args.QF <- function(X=NULL, y=NULL, G=NULL, A=NULL, model=NULL,
                          intercept=NULL, beta.init=NULL, split=NULL, lambda=NULL,
                          mu=NULL, prob.filter=0.05, rescale=NULL, tau=NULL, alpha=NULL, verbose=NULL){
  if(is.null(X) || !is.numeric(X)) stop("X must be a numeric matrix")
  if(is.null(y) || !is.numeric(y)) stop("y must be a numeric vector")
  if(nrow(X) != length(y)) stop("nrow(X) and length(y) must match")
  if(is.null(G) || !is.numeric(G)) stop("G must be a numeric vector")
  if(any(G<1) || any(G>ncol(X)) || length(G)!=length(unique(G))) stop("G must be distinct indexes between 1 and ncol(X)")
  if(!is.null(A)) if(!is.numeric(A) || nrow(A)!=ncol(A) || nrow(A)!=length(G)) stop("A must be a numeric square matrix")
  if(is.null(model) || !model%in%c("linear","logistic","logistic_alter","probit")){
    stop("specified model has not been developed yet")
  }
  if(is.null(intercept) || length(intercept)!=1 || !is.logical(intercept)){
    stop("intercept must be a Boolean")
  }
  if(!is.null(beta.init)){
    if(!is.numeric(beta.init)) stop("beta.init must be numeric")
    if(is.vector(beta.init)){
      if(intercept) if(length(beta.init)!=(ncol(X)+1)) stop("intercept is TRUE, beta.init must be length of ncol(X)+1")
      if(!intercept) if(length(beta.init)!=ncol(X)) stop("intercept is FALSE, beta.init must be length of ncol(X)")
    }
    if(is.matrix(beta.init)){
      if(ncol(beta.init)!=1) stop("If beta.init is a matrix, ncol(beta.init) must be 1")
      if(intercept) if(nrow(beta.init)!=(ncol(X)+1)) stop("intercept is TRUE, beta.init must be length of ncol(X)+1")
      if(!intercept) if(nrow(beta.init)!=ncol(X)) stop("intercept is FALSE, beta.init must be length of ncol(X)")
    }
  }
  if(is.null(split) || length(split)!=1 || !is.logical(split)){
    stop("split must be a Boolean")
  }
  if(!is.null(lambda)){
    if(length(lambda)!=1) stop("lambda must be length of 1")
    if(!is.numeric(lambda)) if(!(lambda %in% c("CV.min","CV"))) stop("lambda must be a number, except from 'CV.min','CV'.")
  }
  if(!is.null(mu)){
    if(length(mu)!=1) stop("mu must be length of 1")
    if(!is.numeric(mu)) stop("mu must be number")
    if(mu<=0) stop("mu must be positive")
  }
  if(is.null(rescale) || !is.numeric(rescale) || length(rescale)!=1) stop("rescale must be numeric")
  if(is.null(prob.filter) || !is.numeric(prob.filter) || length(prob.filter)!=1 || prob.filter < 0 || prob.filter > 0.5) stop("prob.filter must be numeric in (0,0.5)")
  if(is.null(tau) || !is.numeric(tau) || any(tau < 0)) stop("tau must be numeric")
  if(is.null(alpha) || length(alpha)!= 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1){
    stop("alpha must be a number between 0 and 1")
  }
  if(is.null(verbose) || !is.logical(verbose) || length(verbose)!=1 ) stop("verbose must be a Boolean")
}

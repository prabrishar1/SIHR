check.args.LF <- function(X=NULL, y=NULL, loading.mat=NULL, model=NULL, intercept=NULL, intercept.loading=NULL,
                          beta.init=NULL, lambda=NULL, mu=NULL, prob.filter=NULL, rescale=NULL, alpha=NULL, verbose=NULL){
  if(is.null(X) || !is.numeric(X)) stop("X must be a numeric matrix")
  if(is.null(y) || !is.numeric(y)) stop("y must be a numeric vector")
  if(nrow(X) != length(y)) stop("nrow(X) and length(y) must match")
  if(is.null(loading.mat) || !is.numeric(loading.mat)) stop("loading must be a numeric matrix")
  if(ncol(X) != nrow(loading.mat)){
    if(ncol(X) != (nrow(loading.mat)-1)) stop("ncol(X) and nrow(loading) must match")
  }
  if(is.null(model) || !model%in%c("linear","logistic","logistic_alter","probit")){
    stop("specified model has not been developed yet")
  }
  if(is.null(intercept) || length(intercept)!=1 || !is.logical(intercept)){
    stop("intercept must be a Boolean")
  }
  if(is.null(intercept.loading) || length(intercept.loading)!=1 || !is.logical(intercept.loading)){
    stop("intercept.loading must be a Boolean")
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
                          mu=NULL, prob.filter=NULL, rescale=NULL, tau=NULL, alpha=NULL, verbose=NULL){
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

check.args.InnProd <- function(X1=NULL, y1=NULL, X2=NULL, y2=NULL, G=NULL, A = NULL, model = NULL, intercept = NULL,
                               beta.init1 = NULL, beta.init2 = NULL, split = NULL, lambda = NULL, mu = NULL, prob.filter = NULL,
                               rescale = NULL, tau=NULL, alpha = NULL, verbose=NULL){
  if(is.null(X1) || !is.numeric(X1)) stop("X1 must be a numeric matrix")
  if(is.null(X2) || !is.numeric(X2)) stop("X2 must be a numeric matrix")
  if(is.null(y1) || !is.numeric(y1)) stop("y1 must be a numeric vector")
  if(is.null(y2) || !is.numeric(y2)) stop("y2 must be a numeric vector")
  if(nrow(X1) != length(y1)) stop("nrow(X1) and length(y1) must match")
  if(nrow(X2) != length(y2)) stop("nrow(X2) and length(y2) must match")
  if(ncol(X1) != ncol(X2)) stop("ncol(X1) and ncol(X2) must match")
  if(is.null(G) || !is.numeric(G)) stop("G must be a numeric vector")
  if(any(G<1) || any(G>ncol(X1)) || length(G)!=length(unique(G))) stop("G must be distinct indexes between 1 and ncol(X)")
  if(!is.null(A)) if(!is.numeric(A) || nrow(A)!=ncol(A) || nrow(A)!=length(G)) stop("A must be a numeric square matrix")
  if(is.null(model) || !model%in%c("linear","logistic","logistic_alter")){
    stop("specified model has not been developed yet")
  }
  if(is.null(intercept) || length(intercept)!=1 || !is.logical(intercept)){
    stop("intercept must be a Boolean")
  }
  if(!is.null(beta.init1)){
    if(!is.numeric(beta.init1)) stop("beta.init1 must be numeric")
    if(is.vector(beta.init1)){
      if(intercept) if(length(beta.init1)!=(ncol(X1)+1)) stop("intercept is TRUE, beta.init1 must be length of ncol(X1)+1")
      if(!intercept) if(length(beta.init1)!=ncol(X1)) stop("intercept is FALSE, beta.init1 must be length of ncol(X1)")
    }
    if(is.matrix(beta.init1)){
      if(ncol(beta.init1)!=1) stop("If beta.init1 is a matrix, ncol(beta.init1) must be 1")
      if(intercept) if(nrow(beta.init1)!=(ncol(X1)+1)) stop("intercept is TRUE, beta.init1 must be length of ncol(X1)+1")
      if(!intercept) if(nrow(beta.init1)!=ncol(X1)) stop("intercept is FALSE, beta.init1 must be length of ncol(X1)")
    }
  }
  if(!is.null(beta.init2)){
    if(!is.numeric(beta.init2)) stop("beta.init2 must be numeric")
    if(is.vector(beta.init2)){
      if(intercept) if(length(beta.init2)!=(ncol(X2)+1)) stop("intercept is TRUE, beta.init2 must be length of ncol(X2)+1")
      if(!intercept) if(length(beta.init2)!=ncol(X2)) stop("intercept is FALSE, beta.init2 must be length of ncol(X2)")
    }
    if(is.matrix(beta.init2)){
      if(ncol(beta.init2)!=1) stop("If beta.init2 is a matrix, ncol(beta.init2) must be 1")
      if(intercept) if(nrow(beta.init2)!=(ncol(X2)+1)) stop("intercept is TRUE, beta.init2 must be length of ncol(X2)+1")
      if(!intercept) if(nrow(beta.init2)!=ncol(X2)) stop("intercept is FALSE, beta.init2 must be length of ncol(X2)")
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

check.args.Dist = check.args.InnProd


n=200
## setting
beta.setting = 1 # {1,2,3,4}
## cov.setting
cov.setting = 1 # {1,2}
## sim.round
sim.round = 1 #{1,2,3,4,5}
## nsim
nsim = 2

p = 200
delta.vec = c(0.05, 0.1, 0.2, 0.5)
delta = delta.vec[beta.setting]
beta = rep(0, p)
beta[25:50] = delta

A1gen <- function(rho, p){
  M = matrix(NA, nrow=p, ncol=p)
  for(i in 1:p) for(j in 1:p) M[i, j] = rho^{abs(i-j)}
  M
}
if(cov.setting==1) Cov <- A1gen(0.5, p)/2
if(cov.setting==2) Cov <- diag(p)*1.5

test.set = c(40:60)
truth1 = as.numeric(t(beta[test.set])%*%Cov[test.set,test.set]%*%beta[test.set])
truth2 = sum(beta[test.set]^2)

### store infos ###
tau.vec= c(0, 0.5, 1, 2)
ci.mat.1 = ci.mat.2 = ci.mat.3 = ci.mat.4 = matrix(0, nrow=nsim, ncol=length(tau.vec))

verbose=TRUE
set.seed(0)
for(i.sim in 1:nsim){
  print(i.sim)
  X = MASS::mvrnorm(n, rep(0, p), Cov)
  y = X%*%beta + rnorm(n)
  cat("Est1\n")
  Est1<-QF(X,y,G=test.set, A=NULL, model="linear", intercept=TRUE, tau.vec=tau.vec, verbose=verbose)
  cat("Est2\n")
  Est2<-QF(X,y,G=test.set, A=NULL, model="linear", intercept=FALSE, tau.vec=tau.vec, verbose=verbose)
  cat("Est3\n")
  Est3<-QF(X,y,G=test.set, A=diag(length(test.set)), model="linear", intercept=TRUE, tau.vec=tau.vec, verbose=verbose)
  cat("Est4\n")
  Est4<-QF(X,y,G=test.set, A=diag(length(test.set)), model="linear", intercept=FALSE, tau.vec=tau.vec, verbose=verbose)
  ci.mat.1[i.sim,] = (Est1$ci.mat[,1] < truth1) * (Est1$ci.mat[,2] > truth1)
  ci.mat.2[i.sim,] = (Est2$ci.mat[,1] < truth1) * (Est2$ci.mat[,2] > truth1)
  ci.mat.3[i.sim,] = (Est3$ci.mat[,1] < truth2) * (Est3$ci.mat[,2] > truth2)
  ci.mat.4[i.sim,] = (Est4$ci.mat[,1] < truth2) * (Est4$ci.mat[,2] > truth2)
}

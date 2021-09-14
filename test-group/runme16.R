rho= 0.1
n = 10
N=100
Sigma = matrix(rho, n, n)
diag(Sigma) = 1

library(mvtnorm)
x = rmvnorm(100, sigma=Sigma)
y = c(t(x))
idx = rep(1, n*N)
g.idx = rep(1:n, rep=N)
replicate = rep(1:N, each = n)

r = (inla(y ~ -1 + f(idx, model="iid", group = g.idx,
                     replicate = replicate, 
                     hyper = list(prec = list(initial = 0,  fixed=TRUE)), 
                     control.group = list(model = "exchangeable")), 
          data = data.frame(idx, g.idx, replicate),
          control.family = list(hyper = list(prec = list(initial = 12,  fixed=TRUE))), 
          verbose=TRUE))


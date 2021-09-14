n = 10
ng = 5
N = n * ng

y = rnorm(N)
idx = rep(1:n, rep = ng)
g = rep(1:ng, each = n)

r = inla(y ~ -1 + f(idx, group = g, control.group = list(model='iid')),  verbose=TRUE, data = data.frame(y, idx, g))

iidx = 1:N
rr = inla(y ~ -1 + f(iidx, control.group = list(model='iid')),  verbose=FALSE, data = data.frame(y, iidx))

r$mlik
rr$mlik

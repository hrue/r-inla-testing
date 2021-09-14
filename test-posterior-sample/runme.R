n = 100
K = 2
N = K * n
Y = matrix(NA,  N, K)
for(k in 1:K)
    Y[1:n + (k-1)*n, k] = rnorm(n)

xx = rep(1:K, each=n)
yy = xx
r = inla(Y ~ -1 + f(xx) + f(yy, copy="xx"),
        data = list(Y=Y, xx=xx, yy=yy),
        control.compute = list(config=TRUE), 
        family = rep("gaussian", K))

x = inla.posterior.sample(100, r)


n = 10
K = 2
N = K * n
Y = matrix(NA,  N, K)
for(k in 1:K)
    Y[1:n + (k-1)*n, k] = rnorm(n)

xx = rep(1:K, each=n)
yy = xx
A = diag(N)
r = inla(Y ~ 1 + f(xx) + f(yy, copy="xx"),
        data = list(Y=Y, xx=xx, yy=yy),
        control.compute = list(config=TRUE),
        ##control.predictor = list(A=A), 
        family = rep("gaussian", K))

x = inla.posterior.sample(10, r, intern=F, add.names=F)

fun = function(...) {
    mean(xx)/(1 + exp(mean(yy)))
}

fun = function(...) {
    xx[1]
}

fun = function(...) {
    return (theta)
}

fun = function(...) {
    return (c(Intercept, Intercept+1))
}

xx = inla.posterior.sample.eval(fun, x)


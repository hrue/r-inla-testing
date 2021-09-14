library(statmod)
library(sn)

int.fun.sn <- function(fun, skew = 0) {
    par <- unlist(INLA:::inla.sn.reparam(moments = c(0, 1, skew)))
    int.fun <- function(x) {
        return (dsn(x, dp = par) * fun(x))
    }
    return(integrate(int.fun, lower = -10, upper = 10, rel.tol = 1e-12, subdivisions = 1000L))
}

gq.sn <- function(n = 11, skew = 0) {
    par <- unlist(INLA:::inla.sn.reparam(moments = c(0, 1, skew)))
    gq <- gauss.quad.prob(n, dist = "normal")
    gq$weights <- gq$weights * dsn(gq$nodes, dp = par) / dnorm(gq$nodes)
    plot(gq$nodes, log(gq$weights))
    return(gq)
}
eval.fun <- function(fun, n = 11) {
    gq <- gauss.quad.prob(n, dist = "normal")
    return (sum(gq$weights * fun(gq$nodes)))
}

eval.fun.sn <- function(fun, n = 11, skew = 0) {
    gq <- gq.sn(n, skew)
    return (sum(gq$weights * fun(gq$nodes)))
}

eval.fun(function (x) 1)
eval.fun(function (x) x)
eval.fun(function (x) x^2)
eval.fun(function (x) x^4)

s <- 0.3
eval.fun.sn(function (x) 1, skew = s)
eval.fun.sn(function (x) x, skew = s)
eval.fun.sn(function (x) x^2, skew = s)
eval.fun.sn(function (x) x^3, skew = s)
eval.fun.sn(function (x) x^4, skew = s)
int.fun.sn(function (x) x^4, skew = s)

f <- numeric(3)
ds <- 1e-4
f[1] <- int.fun.sn(function (x) sin(x), skew = s-ds)[[1]]
f[2] <- int.fun.sn(function (x) sin(x), skew = s)[[1]]
f[3] <- int.fun.sn(function (x) sin(x), skew = s+ds)[[1]]
print(c(f[2],  (f[3]-f[1])/(2*ds), (f[3]-2*f[2]+f[1])/ds^2))

library(numDeriv)
fun <- function(s) int.fun.sn(function (x) sin(x), skew = s)[[1]]
print(c(fun(s), grad(fun, s), hessian(fun, s)[1, 1]))

system("inla.mkl.work -m testit 55|grep ^sin" )

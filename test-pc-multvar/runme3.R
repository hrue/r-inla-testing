## parallel version of runme2.R
library(parallel)
nsim = 1000
ntimes = 100
n = 123
H = rep(10, n)
H = Diagonal(n, x = rep(10, n))
lambda = 1.32
h = function(...) INLA:::inla.pc.multvar.h.default(...)
dd = unlist(mclapply(1:ntimes,
    function(idx) {
        x = inla.pc.multvar.sphere.r(nsim, H=H, lambda = lambda)
        d = numeric(nsim)
        for(i in 1:nsim) {
            xx = x[i, ]
            if (is.matrix(H) || is(H, class(Diagonal(1)))) {
                xx = matrix(xx, n, 1)
                H = as.matrix(H)
                d[i] = h(0.5* t(xx) %*% H %*% xx)
            } else {
                d[i] = h(0.5*sum(xx^2 * H))
            }
        }
        return (d)
    }))
hist(log(dd), prob=TRUE, n=100, main = paste("hist(log(dist)) for lambda =", lambda))
xx = seq(0.00000001, max(dd), len=100000)
lines(log(xx), dexp(xx, rate = lambda)*xx, lwd=2)
print(c(estimate = mean(dd),  truth = 1/lambda))



nsim = 1000
ntimes = 100
n = 123
H = matrix(0, n, n)
diag(H) = 10
H = rep(10, n)
H = Diagonal(n, x = rep(10, n))
lambda = .2
h = function(...) INLA:::inla.pc.multvar.h.default(...)
dd = c()
for(nt in 1:ntimes) {
    x = inla.pc.multvar.sphere.r(nsim, H=H, lambda = lambda)
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
    dd = c(dd, d)
}
hist(dd, prob=TRUE, n=100, main = paste("lambda =", lambda))
xx = seq(0, max(dd), len=1000)
lines(xx, dexp(xx, rate = lambda))
print(c(estimate = mean(dd),  truth = 1/lambda))



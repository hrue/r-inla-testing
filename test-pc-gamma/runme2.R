my.pc.mgamma = function(theta, param, log=FALSE) {
    x = exp(-theta)
    ld = inla.pc.dgamma(x, lambda = param[1], log=TRUE) - theta
    return (if (log) ld else exp(ld))
}
my.pc.gamma = function(theta, param, log=FALSE) {
    x = exp(theta)
    ld = inla.pc.dgamma(x, lambda = param[1], log=TRUE) + theta
    return (if (log) ld else exp(ld))
}

n = 10^6
par(mfrow=c(2, 1))
xx = seq(-10, 10, len=1000)
lambda = runif(1, min=1, max=3)
r = inla.pc.rgamma(n, lambda = lambda)
rr = -log(r)
hist(rr,  prob=T, n=300)
lines(xx, my.pc.mgamma(xx, param = lambda), lwd=3, col="blue")

xx = seq(-10, 10, len=1000)
lambda = runif(1, min=1, max=3)
r = inla.pc.rgamma(n, lambda = lambda)
rr = log(r)
hist(rr,  prob=T, n=300)
lines(xx, my.pc.gamma(xx, param = lambda), lwd=3, col="blue")




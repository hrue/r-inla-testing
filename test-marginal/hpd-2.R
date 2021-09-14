x = seq(-1,5,len=1000)
m = list(x = x, y = dnorm(x))


par(mfrow=c(3, 3))
k = 1
for(p in (1:50/51)) {
    plot(m,  type="l")
    r = inla.hpdmarginal(p, m)
    abline(v = r)
    abline(h = inla.dmarginal(r[2], m))
    k = k + 1
    if (k == 9) {
        k = 1
        dev.new()
        par(mfrow=c(3, 3))
    }
}


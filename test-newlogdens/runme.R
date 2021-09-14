
if (FALSE) {
    n = 6
    i = 1:n
    y = i + rnorm(n)

    m = n -2
    extraconstr = list(A = matrix(runif(n*m), m, n), e = 0+runif(m))

    r = inla(y ~ 1 + f(i, constr=T, extraconstr=extraconstr),
            control.data = list(initial=10,fixed=T), data = data.frame(y))
    print( extraconstr$A %*% r$summary.random$i$mean - extraconstr$e)

    rr = inla(y ~ 1 + f(i, constr=T, extraconstr = extraconstr),
            control.data = list(initial=10,fixed=T), data = data.frame(y),
            inla.call="inla.work", control.inla = list(step.len=0.00001))

    print( extraconstr$A %*% rr$summary.random$i$mean - extraconstr$e)
}
if (FALSE) {
    n = 100
    y = rpois(n, lambda = exp(rnorm(n) + 2))
    i = 1:n

    r = inla(y ~ 1+f(i, constr=T), family = "poisson", data = data.frame(y))
    rr = inla(y ~ 1+f(i, constr=T), family = "poisson", data = data.frame(y),
            inla.call="inla.work", control.inla=list(step.len=-2))
}
if (TRUE) {
    sd = 2
    n = 100
    intercept = 0.5
    eta = intercept+rnorm(n, sd=sd)
    p = exp(eta)/(1+exp(eta))
    y = rbinom(n, 1, prob = p)
    i = 1:n

    formula = y ~ 1 + f(i, param = c(0.01,0.01), constr=T)
    r = inla( formula, family = "binomial", Ntrials = rep(1,n),
            data = data.frame(y))
    r1 = inla( formula, family = "binomial", Ntrials = rep(1,n),
            data = data.frame(y), control.inla = list(step.len = -1*sd, verbose=F),
            inla.call="inla.work", control.mode = list(result = r, restart=T), verbose=F)
    r2 = inla( formula, family = "binomial", Ntrials = rep(1,n),
            data = data.frame(y), control.inla = list(step.len = -2*sd, verbose=F),
            inla.call="inla.work", control.mode = list(result = r, restart=T), verbose=F)
    r3 = inla( formula, family = "binomial", Ntrials = rep(1,n),
            data = data.frame(y), control.inla = list(step.len = -2.5*sd, verbose=F),
            inla.call="inla.work", control.mode = list(result = r, restart=T), verbose=F)
}

m = r$internal.marginals.hyperpar[[1]]
m1 = r1$internal.marginals.hyperpar[[1]]
m2 = r2$internal.marginals.hyperpar[[1]]
m3 = r3$internal.marginals.hyperpar[[1]]
plot(m, xlim = range(c(m[, "x"],m3[,"x"])), ylim = c(range(c(m[,"y"], m3[,"y"]))))
lines(m1, lty=2)
lines(m2, lty=3)
lines(m3, lty=4)
lines(rep(log(1/sd^2),2), c(0, 1000000),col="red",lwd=3)

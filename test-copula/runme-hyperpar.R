n = 100
u = rnorm(n, sd = 1)
v = rnorm(n, sd = .01)
w = rnorm(n, sd = .01)
idx = 1:n
idx2 = rep(1:10, n)[1:n]
idx3 = rep(1:20, n)[1:n]
eta = -2 + u[idx] + v[idx2] + w[idx3]
p = 1/(1+exp(-eta))
y = rbinom(n,  size=1, prob = p)

hyper = list(prec = list(param =c(1, 1)))

for (nt in 1:10) {
    r = inla(y ~ 1 + f(idx, hyper = hyper) + f(idx2, hyper = hyper) + f(idx3, hyper = hyper), 
        data = data.frame(y, idx, idx2, idx3),
        family = "binomial", 
        control.inla = list(
            correct = TRUE,
            skip.configurations = FALSE, 
            tolerance = 1E-10))
    rr = inla.hyperpar(r,  diff.logdens = 20, dz=1.0, keep=T)
    s = inla(y ~ 1 + f(idx, hyper = hyper) + f(idx2, hyper = hyper)  + f(idx3, hyper = hyper), 
        data = data.frame(y, idx, idx2, idx3),
        family = "binomial", 
        control.inla = list(
            correct = FALSE, 
            skip.configurations = FALSE, 
            tolerance = 1E-10))
    ss = inla.hyperpar(s,  diff.logdens = 20, dz=1.0)
    z = inla(y ~ 1 + f(idx, hyper = hyper) + f(idx2, hyper = hyper)  + f(idx3, hyper = hyper),  
        data = data.frame(y, idx, idx2, idx3),
        family = "binomial", 
        control.inla = list(
            correct = TRUE, 
            skip.configurations = FALSE, 
            tolerance = 1E-10),
        num.threads = 1)
    zz = inla.hyperpar(z,  diff.logdens = 20, dz=1.0)
    print(nt)
    print(signif(rr$summary.hyperpar))
    print(signif(ss$summary.hyperpar))
    print(signif(zz$summary.hyperpar))
    if (nt == 1) {
        plot(inla.smarginal(rr$internal.marginals.hyperpar[[1]]), type="l", col = "red")
        lines(inla.smarginal(ss$internal.marginals.hyperpar[[1]]), type="l", col = "blue")
    } else {
        lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]), col = "red")
        lines(inla.smarginal(ss$internal.marginals.hyperpar[[1]]), col = "blue")
    }
}

        

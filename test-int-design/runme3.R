n = 50
x = rnorm(n)
y = 1 + x + rnorm(n, sd = 0.2)
param = c(1, 0.04)
dz = 0.1
cpu.time = numeric(3)
cpu.time[1] = Sys.time()
r.std = inla(y ~ 1 + x, data = data.frame(y, x),
             control.inla = list(int.strategy = "grid",
                                 dz = dz,
                                 diff.logdens = 8), 
             control.family = list(
                 hyper = list(
                     prec = list(
                         prior = "loggamma",
                         param = param))))
cpu.time[1] = Sys.time() - cpu.time[1]

s = r.std$internal.summary.hyperpar[1,"sd"]
m = r.std$internal.summary.hyperpar[1,"mean"]
theta = m + s*seq(-4, 4, by = dz)
weight = dnorm(theta,  mean = m, sd = s)

cpu.time[2] = Sys.time()
r = rep(list(list()), length(theta))
for(k in seq_along(r)) {
    r[[k]] = inla(y ~ 1 + x,
                  control.family = list(
                      hyper = list(
                          prec = list(
                              initial = theta[k],
                              fixed=TRUE))),
                  data = data.frame(y, x))
}
r.merge = inla.merge(r, prob = weight)
cpu.time[2] = Sys.time() - cpu.time[2]

cpu.time[3] = Sys.time()
r.design = inla(y ~ 1 + x,
                data = data.frame(y, x),
                control.family = list(
                    hyper = list(
                        prec = list(
                            ## the prior here does not really matter, as we will override
                            ## it with the user.expert in any case.
                            prior = "pc.prec",
                            param = c(1, 0.01)))), 
                control.inla = list(int.strategy = "user.expert",
                                    int.design = cbind(theta, weight)))
cpu.time[3] = Sys.time() - cpu.time[3]
names(cpu.time) = c("plain", "merge", "design")
print(cpu.time)

par(mfrow = c(1, 2))
for(k in 1:2) {
    plot(inla.smarginal(r.std$marginals.fixed[[k]]),
         lwd = 2, lty = 1, type = "l", 
         xlim = inla.qmarginal(c(0.0001, 0.9999), r.std$marginals.fixed[[k]]))
    lines(inla.smarginal(r.design$marginals.fixed[[k]]),
          lwd = 2, col = "blue", lty = 1)
    lines(inla.smarginal(r.merge$marginals.fixed[[k]]),
          lwd = 2, col = "yellow", lty = 1)
}

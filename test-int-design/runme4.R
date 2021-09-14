n = 10
x = rnorm(n)
y = 1 + x + rnorm(n, sd = 0.2)
param = c(1, 0.04)
dz = 0.25
inla.setOption(num.threads=8)

r.std = inla(y ~ 1 + x, data = data.frame(y, x),
             control.inla = list(int.strategy = "grid",
                                 dz = dz,
                                 diff.logdens = 8), 
             control.compute = list(po=TRUE, cpo=TRUE), 
             control.family = list(
                 hyper = list(
                     prec = list(
                         prior = "loggamma",
                         param = param))))

s = r.std$internal.summary.hyperpar[1,"sd"]
m = r.std$internal.summary.hyperpar[1,"mean"]
theta = m + s*seq(-4, 4, by = dz)
##theta = m + s*c(-1, 0, 1)
weight = dnorm(theta,  mean = m, sd = s)

r = rep(list(list()), length(theta))
for(k in seq_along(r)) {
    r[[k]] = inla(y ~ 1 + x,
                  control.family = list(
                      hyper = list(
                          prec = list(
                              initial = theta[k],
                              fixed=TRUE))),
                  control.compute = list(po=TRUE, cpo=TRUE), 
                  data = data.frame(y, x))
}
r.merge = inla.merge(r, prob = weight)

r.design = inla(y ~ 1 + x,
                data = data.frame(y, x),
                control.family = list(
                    hyper = list(
                        prec = list(
                            ## the prior here does not really matter, as we will override
                            ## it with the user.expert in any case.
                            prior = "pc.prec",
                            param = c(1, 0.01)))), 
                control.compute = list(po=TRUE, cpo=TRUE), 
                control.inla = list(int.strategy = "user.expert",
                                    int.design = cbind(theta, weight)))


A = cbind(r.merge$po$po,  r.design$po$po)
print(mean(abs(A[, 1]-A[, 2])))
B = cbind(r.merge$cpo$cpo,  r.design$cpo$cpo)
print(mean(abs(B[, 1]-B[, 2])))


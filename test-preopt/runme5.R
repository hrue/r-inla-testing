INLA:::inla.my.update()
INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")

n = 200
x = inla.group(runif(n, min = -10, max = 10))
y = 1 + x + rnorm(n, sd = 0.2)
param = c(1, 0.04)
dz = 0.75
inla.setOption(num.threads=1)

formula <- y ~ 1 + f(x, model = "rw2", scale.model = TRUE)

r.std = inla(formula, 
             data = data.frame(y, x),
             control.expert = list(disable.gaussian.check = TRUE), 
             control.inla = list(int.strategy = "grid",
                                 dz = dz,
                                 diff.logdens = 8), 
             control.compute = list(po=TRUE, cpo=TRUE), 
             verbose = TRUE, keep = TRUE, 
             control.family = list(
                 hyper = list(
                     prec = list(
                         prior = "loggamma",
                         param = param))),
             inla.arg = "-v -t4:1 -b -P")

stop("XXXXXXXXX" )

r = rep(list(list()), nrow(r.std$joint.hyper))
for(k in seq_along(r)) {
    r[[k]] = inla(y ~ 1 + x,
                  control.family = list(
                      hyper = list(
                          prec = list(
                              initial = r.std$joint.hyper[k, 1], 
                              fixed=TRUE))),
                  control.compute = list(po=TRUE, cpo=TRUE), 
                  data = data.frame(y, x))
}
r.merge = inla.merge(r, prob = r.std$joint.hyper[, 3])

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
                                    int.design = cbind(r.std$joint.hyper[, 1], r.std$joint.hyper[, 3])))


A = cbind(r.merge$po$po,  r.design$po$po)
print(mean(abs(A[, 1]-A[, 2])))
B = cbind(r.merge$cpo$cpo,  r.design$cpo$cpo)
print(mean(abs(B[, 1]-B[, 2])))

par(mfrow = c(1, 2))
plot(inla.smarginal(r.std$marginals.fixed$x), pch = 19)
lines(inla.smarginal(r.design$marginals.fixed$x), col = "blue", lwd = 5)
lines(inla.smarginal(r.merge$marginals.fixed$x), col = "red", lwd = 3)
plot(inla.smarginal(r.std$marginals.fixed$'(Intercept)'), pch = 19)
lines(inla.smarginal(r.design$marginals.fixed$'(Intercept)'), col = "blue", lwd = 5)
lines(inla.smarginal(r.merge$marginals.fixed$'(Intercept)'), col = "red", lwd = 3)

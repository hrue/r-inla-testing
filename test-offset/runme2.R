
inla.setOption("inla.call", "inla.work")
inla.my.update()

n = 10
off = rep(100, n)
y = off + rnorm(n)

formula = y ~ 1
r = inla(formula, data = data.frame(off,y), control.predictor = list(compute=T))

fformula = y ~ 1 + offset(off)
rr = inla(fformula, data = data.frame(off,y), control.predictor = list(compute=T))

print(cbind(r$summary.linear.predictor$mean,
            rr$summary.linear.predictor$mean))

print(cbind(r$summary.linear.predictor$sd,
            rr$summary.linear.predictor$sd))
            

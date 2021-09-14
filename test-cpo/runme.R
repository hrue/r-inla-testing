n = 100
s = 0.01
x = sort(runif(n))
y = 1 + x + rnorm(n, sd = s)
y[1] = y[1] + 10 * s
r = inla(y~1+x, data = data.frame(x, y),
        family = "gaussian", 
        control.compute = list(cpo=TRUE))

system.time({rr = inla.cpo(r, force=T)})

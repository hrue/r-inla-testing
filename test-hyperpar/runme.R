
n = 1000
idx = 1:n
iidx = 1:n
y = rnorm(n)

r = inla(y ~ 1 + f(idx) + f(iidx),
         data = data.frame(y, idx, iidx),
         control.compute = list(config=TRUE))
rr = inla.hyperpar(r)


n = 10
off = 1:n + 99
z = rnorm(n,sd=0.1)
y = rnorm(n,sd=0.1) + off + z

formula = y ~ z + offset(off)
r = inla(formula, data = data.frame(z,y),
        control.predictor = list(compute=TRUE),
        keep=T, inla.call="inla.work",
        inla.arg="-m mcmc -v -N10 -T1 -F -Y")


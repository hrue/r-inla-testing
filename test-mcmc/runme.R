##inla.setOption("inla.call", "inla.work")
n = 10
y = rnorm(n) + 1:n
x = rep(1:(n/2), each = 2)
z = 1:n

formula = y ~ offset(z) + f(x)
r = inla(formula, data = data.frame(x,y),
        control.predictor = list(compute=T),
        keep=T, inla.call="inla.work", inla.arg="-m mcmc -v -N3 -T1 ")


n = 1500
y = rpois(n, lambda = exp(1 + rnorm(n)))
formula = y ~ 1 + z + f(i,  model="ar1") + f(j)
i = 1:n
j = sample(1:10, n,  replace=TRUE)
z = rnorm(n)
data = data.frame(y, z, i, j)

r = inla(formula,  data = data, family = "poisson",
        control.inla = list(strategy = "laplace", cutoff = 0),
        verbose=TRUE,
        inla.call = "/home/hrue/p/inla/work/local/bin/inla.ref")

rr = inla(formula,  data = data, family = "poisson",
        control.inla = list(strategy = "laplace"),
        verbose=TRUE,
        inla.call = "inla.work",
        keep=TRUE)




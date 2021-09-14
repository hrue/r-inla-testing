n = 100
y = rpois(n, lambda = exp(rnorm(n)))
x = rnorm(n)
r = inla(y ~f(x), data = data.frame(x,y), family = "poisson", keep=TRUE)

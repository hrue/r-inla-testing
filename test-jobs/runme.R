n = 100
y = rnorm(n)
r = inla(y ~ 1,  data = data.frame(y),
    inla.call = "submit",  verbose=TRUE)


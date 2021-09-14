n = 1000
z = rnorm(n)
eta = 5 + z
y = rpois(n, lambda = exp(eta))
Y = matrix(NA, n, 2)
n2 = n %/% 2L
Y[1:n2, 1] = y[1:n2]
Y[n2 + 1:n2, 2] = y[n2 + 1:n2]

r = inla(Y ~ 1 + z,
        data = list(Y=Y, z=z), 
        family = c("poisson", "poisson"), 
        control.predictor = list(compute=TRUE, link = 1),
        keep=TRUE, verbose=TRUE, 
        inla.call="inla.work")

rr = inla(Y ~ 1 + z,
        data = list(Y=Y, z=z), 
        family = c("poisson", "poisson"), 
        control.predictor = list(compute=TRUE, link = 1),
        inla.call = "inla")




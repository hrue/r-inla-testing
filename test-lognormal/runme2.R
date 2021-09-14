n = 300
x = c(scale(runif(n)))
eta = 1+2.2*x
y = exp(rnorm(n, mean = eta,  sd = 1))
data = data.frame(y, x)
formula = y  ~ 1 + x
r=inla(formula, family ="lognormal", data=data,
       control.predictor = list(compute=TRUE))



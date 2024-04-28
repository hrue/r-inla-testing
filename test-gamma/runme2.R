n = 1000
x = rnorm(n, sd = 0.1)
eta = 1 + x
mu = exp(eta)
prec.scale = runif(n, min = 0.99, max = 1.01)
prec.par = 1.2
a = prec.par * prec.scale
b = mu / (prec.par * prec.scale)
y = rgamma(n, shape = a, scale = b)
r = inla(y ~ 1 + x,
         data = list(y = y, x = x),
         scale = prec.scale,
         family = "gamma",
         control.inla = list(cmin = 0), 
         verbose = TRUE, 
         control.family = list(control.link = list(model = "quantile", quantile = 0.5)))

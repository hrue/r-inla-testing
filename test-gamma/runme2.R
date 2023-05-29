inla.setOption(safe = FALSE)
n = 10
x = rnorm(n, sd = 0.1)
eta = 1 + x
mu = exp(eta)
prec.scale = runif(n, min = 0.99, max = 1.01)
prec.par = 1.2
a = prec.par * prec.scale
b = mu / (prec.par * prec.scale)
y = rgamma(n, shape = a, scale = b)
event <- rep(1, n)
Y <- inla.surv(y, event)
r = inla(y ~ 1 + x,
         data = list(Y = Y, x = x),
         scale = prec.scale,
         family = "gamma",
         verbose = TRUE, 
         control.family = list(control.link = list(model = "quantile", quantile = 0.5)))
if (F)rr = inla(Y ~ 1 + x,
          data = list(Y = Y, x = x),
          scale = prec.scale,
          family = "gammasurv",
          control.family = list(control.link = list(model = "quantile", quantile = 0.5)))


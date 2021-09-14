n = 100
x = 1+rnorm(n)
xx = 3 + rnorm(n)
y = 1 + x + xx + rnorm(n, sd=.1)
selection = list(xx=1, Predictor = 3:4, x=1)
r = inla(y ~ 1 + x + xx,
         control.compute = list(config=TRUE), 
         data = data.frame(y, x, xx),
         selection = selection)

fun = function(x) Predictor
ns=10
xx = inla.posterior.sample(ns, r, selection)
print(inla.posterior.sample.eval(fun, xx))

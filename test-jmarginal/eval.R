n = 100
x = rnorm(n)
eta = 1 + x
y = eta + rnorm(n, sd=0.1)
selection = list(x = 1, Predictor = c(1, 2, 4, 5),  '(Intercept)' = 1)
r = inla(y ~ 1 + x,
         data = data.frame(y, x),
         selection = selection)
xx = inla.rjmarginal(1000,  r)
xx.eval = inla.rjmarginal.eval(function() c(x, Predictor, Intercept),  xx)

print(cbind(xx$samples[, 1]))
print(cbind(xx.eval[, 1]))

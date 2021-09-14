r = inla(y ~ 1 + f(idx), data = data.frame(y = 1:10, idx = 1:10),
         control.compute = list(config=TRUE))
x = inla.posterior.sample(1, r, selection = list('(Intercept)' = 0, idx = c(-2,-3)))

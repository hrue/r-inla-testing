n <- 10
y = rnorm(n)
x <- letters[sample(1:27, n)]

r <- inla(y ~ -1 + x, data = data.frame(y, x),
          control.family = list(hyper = list(prec = list(initial = 2, fixed = TRUE))))
summary(r)
INLA:::inla.my.update()
rr <- inla(y ~ -1 + x, data = data.frame(y, x), 
          control.family = list(hyper = list(prec = list(initial = 2, fixed = TRUE))))
summary(r)

rr$summary.fixed - r$summary.fixed


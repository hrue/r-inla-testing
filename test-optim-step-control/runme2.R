INLA:::inla.my.update(b = T)
n <- 1000
y <- rnorm(n, sd = 10000)
r <- inla(y ~ 1, data = data.frame(y), verbose = TRUE,
          control.family = list(hyper = list(prec = list(initial = 10))))



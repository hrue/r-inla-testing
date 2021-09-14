
n <- 10
y <- rpois(n, 1)

r <- inla(y ~ 1, data = data.frame(y),
          family = "poisson",
          inla.mode = "experimental",
          control.fixed = list(prec.intercept = 1), 
          verbose = TRUE)


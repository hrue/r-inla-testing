library(INLA)
n <- 30
x <- rnorm(n, sd = 200)
eta <- 1 + x
y <- eta + rnorm(n)

r <- inla(y ~ 1 + x, 
          data = data.frame(y, x),
          family = "stdgaussian", 
          control.inla = list(step.len = 1e-4), 
          control.expert = list(disable.gaussian.check = TRUE), 
          verbose = TRUE)
summary(r)

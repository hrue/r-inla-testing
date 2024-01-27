library(INLA)
n <- 30000
x <- rnorm(n, sd = 100)
eta <- 1 + x
y <- eta + rnorm(n, sd = 10^4)

r <- inla(y ~ 1 + x, 
          data = data.frame(y, x),
          family = "gaussian", 
          control.inla = list(step.len = 1e-2), 
          control.expert = list(disable.gaussian.check = TRUE), 
          verbose = TRUE)
summary(r)

rr <- inla(y ~ 1 + x, 
          data = data.frame(y, x),
          family = "gaussian", 
          control.inla = list(step.len = 1e-2), 
          control.expert = list(disable.gaussian.check = TRUE), 
          verbose = TRUE,
          inla.call = "inla.mkl.work")
summary(rr)

rrr <- inla(y ~ 1 + x, 
          data = data.frame(y, x),
          family = "gaussian", 
          control.inla = list(step.len = 1e-2), 
          control.expert = list(disable.gaussian.check = TRUE), 
          verbose = TRUE,
          inla.call = "inla.mkl.work")
summary(rrr)

r$mlik - rr$mlik
rrr$mlik - rr$mlik

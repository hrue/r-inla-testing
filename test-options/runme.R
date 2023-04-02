n <- 100
x <- rnorm(n)
y <- rpois(n, exp(0 + x + rnorm(n)))
idx <- 1:n

r1 <- inla(y ~ 1 + x + f(idx),
          data = data.frame(y, x, idx),
          family = "poisson")
r2 <- inla(y ~ 1 + x + f(idx),
          data = data.frame(y, x, idx),
          family = "poisson")

rr1 <- inla(y ~ 1 + x + f(idx),
           data = data.frame(y, x, idx),
          family = "poisson",
          control.compute = list(internal.opt = FALSE),
          num.threads = "1:1")
rr2 <- inla(y ~ 1 + x + f(idx),
           data = data.frame(y, x, idx),
          family = "poisson",
          control.compute = list(internal.opt = FALSE),
          num.threads = "1:1")



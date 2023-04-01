n <- 10^3
x <- rnorm(n)
y <- rpois(n, exp(2 + x))

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson")
rr <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.expert = list(dot.product.gain = TRUE))



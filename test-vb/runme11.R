n <- 2
y <- rep(1, n)
r <- inla(y ~ 1,
          data = data.frame(y),
          family = 'poisson',
          control.inla = list(control.vb = list(enable = !FALSE)),
          verbose = TRUE)
r$summary.fixed[1, "mean"]

intercept <- seq(-5, 3, by = 0.0001)
e <- sum(intercept * dpois(y[1], lambda = exp(intercept))^n * diff(intercept)[1])
e

plot(intercept,  dpois(y[1], lambda = exp(intercept))^n)
abline(v = e, lwd = 3)
abline(v = r$summary.fixed[1, "mean"], lwd = 5)


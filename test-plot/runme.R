n <- 100
x <- rnorm(n)
y <- 1 + x + rnorm(n)
idx <- rep(1:5, each = n %/% 5L)


r <- inla(y ~ 1 + x + f(idx), data = data.frame(y, x, idx),
          control.compute = list(cpo = TRUE, waic = TRUE))

plot(r, cex = 1.5, single = TRUE)

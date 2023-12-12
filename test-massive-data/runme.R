n <- 1E6
x <- rnorm(n)
y <- rpois(n, exp(2 + x))

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          verbose = TRUE,
          keep = TRUE)

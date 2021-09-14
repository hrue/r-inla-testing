
n <- 10
x <- rnorm(n)
y <- 1 + x + rnorm(n)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          keep = TRUE,
          verbose = TRUE)

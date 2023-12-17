n <- 1E6
x <- rnorm(n)
y <- 1 + x + rnorm(n)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "stdnormal",
          verbose = TRUE,
          keep = TRUE)

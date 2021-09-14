n <- 10
y <- rnorm(n, 1)
r <- inla(y ~ 1,
          data = data.frame(y),
          verbose = TRUE,
          keep = TRUE)

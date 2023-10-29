n <- 2
m <- 10
A <- matrix(runif(n*m), m, n)
A[1:2, ] <- 0

r <- inla(y ~ 1 + x,
          data = list(y = rnorm(m), x = rnorm(n), A = A),
          control.predictor = list(A = A), 
          safe = FALSE, 
          verbose = TRUE)


n <- 2
m <- 3
A <- matrix(0, m, n)
A[1, ] <- c(1, 1)
A[2, ] <- c(1, 0)
A[3, ] <- c(0, 0)

r <- inla(y ~ -1 + f(idx, model = "iid"),
          data = list(idx = 1:n, y = rep(0, m), A = A), 
          control.predictor = list(A = A),
          verbose = TRUE)

INLA:::inla.my.update(b = T)
n <- 3000
A <- matrix(rnorm(n^2), n, n)
Q <- A %*% t(A)
S <- solve(Q)
S <- S / mean(diag(S))
Q <- solve(S)

library(mvtnorm)
y <- as.vector(rmvnorm(1, sigma = S)) + rnorm(n)
idx <- 1:n

r <- inla(y ~ -1 + f(idx, model = "generic0", Cmatrix=Q),
          data = data.frame(y, idx),
          family = "stdnormal",
          verbose = TRUE,
          safe = FALSE, 
          keep = TRUE,
          inla.call = "")

          

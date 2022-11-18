n <- 5
A <- matrix(runif(n^2), n, n)
A <- A %*% t(A)
m <- 3
for (k in (m+1):n) {
    A[k, ] <- matrix(rnorm(m), 1, m) %*% A[1:3, ]
}

library(pracma)
Anew <- t(orth(t(A)))


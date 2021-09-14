n = 500
ord = 2
nc = 3

Q = n^ord*INLA:::inla.rw(n, ord = ord)

A = matrix(runif(n*nc), nc, n)
e = matrix(rnorm(nc), nc, 1)

A[1, ] = 1/n
A[2, ] = c(scale(1:n))
## Σ ∗ = Q −1 − Q −1 A T (AQ −1 A T ) −1 AQ −1 .

S = INLA:::inla.ginv(Q, rankdef = ord)
SS = S - S %*% t(A) %*% INLA:::inla.ginv(A %*% S %*% t(A)) %*% A %*% S
SS = (SS + t(SS))/2

eig = sort(eigen(SS, only.values=TRUE)$values, decreasing=T)
eig = eig/eig[1]

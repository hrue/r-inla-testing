library(mvtnorm)
n <- 300
nrep <- 30
A <- matrix(rnorm(n^2), n, n)
Q <- A %*% t(A)
S <- solve(Q)
Q <- Q * exp(mean(log(diag(S))))
s <- 0.1
y <- as.vector(t(rmvnorm(nrep, sigma = solve(Q)))) + rnorm(n * nrep, sd = s)
idx <- rep(1:n, nrep)
replicate <- rep(1:nrep, each = n)

r <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = Q, replicate = replicate), 
          data = data.frame(y, idx, replicate),
          family = "gaussian")

exp(r$mode$theta)

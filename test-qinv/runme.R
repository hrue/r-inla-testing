n <- 2000
Q <- matrix(rnorm(n^2), n, n)
Q[, ] <- (runif(n^2) < 0.01) * Q[, ]
Q <- Q %*% t(Q)
diag(Q) <- diag(Q) + 1
Q <- inla.as.sparse(Q)
if (n <= 100) Q

G <- Q
G[Q != 0] <- 1

nc <- 10
constr <- list(A = matrix(rnorm(n * nc), nc, n),
               e = rnorm(nc))

INLA:::inla.my.update(b = T)
inla.setOption(smtp = "taucs")

ret <- inla.qinv(Q, constr, num.threads = 1)

S <- solve(Q)
A <- constr$A
AS <- A %*% S
S <- S - t(AS) %*% solve(AS %*% t(A)) %*% AS
max(abs((S- ret) * G))
sum(G)/n^2

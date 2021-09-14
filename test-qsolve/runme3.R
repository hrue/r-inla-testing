n <- 10
Q <- matrix(rnorm(n^2), n, n)
Q <- Q %*% t(Q)
Q <- (Q+t(Q))/2
stopifnot(all(eigen(Q)$values > 0))

L <- t(chol(Q))
b <- rnorm(n)
B <- matrix(b, n, 1)

print(mean(abs(inla.qsolve(Q, B) - solve(Q, b))))
print(mean(abs(inla.qsolve(Q, B, method = "forward"), - solve(L, b))))
print(mean(abs(inla.qsolve(Q, B, method = "backward"), - solve(t(L), b))))


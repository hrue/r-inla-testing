n <- 6
ubw <- 1
lbw <- 3

A <- matrix(0, n, n)

for(i in 1:n) {
    for(j in 1:n) {
        if (j - i <= ubw && i - j <= lbw) {
            if (i == j) {
                A[i, j] <-  10 + i-1
            } else {
                A[i, j] <-  i-j + 0.1 * (i-1)
            }
        }
    }
}
A

AA <- (A %*% A)
print(A %*% A)
AAA <- (A %*% A %*% A)
print(A %*% A %*% A)

x <- 1:n
A[upper.tri(A)] <- 0
A <- A + t(A)
diag(A) <- diag(A) / 2

print(solve(A, x))

print(inla.as.sparse(t(chol(A))))
Ainv <- solve(A)
for(i in 1:n) {
    for (j in 1:n) {
        if (j < i - lbw || j > i) Ainv[i, j] <- 0
    }
}
print(inla.as.sparse(Ainv))

print(inla.as.sparse(1.1 * A + 2.2 * AA + 3.3 * AAA))

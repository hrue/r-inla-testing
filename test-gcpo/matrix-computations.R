spd <- function(n) {
    A <- matrix(rnorm(n^2), n, n)
    S <- A %*% t(A)
    S <- S / mean(diag(S))
    diag(S) <- diag(S) + 1e-4
    return (S)
}

n <- 5
mu <- rnorm(n)
S <- spd(n)
Q <- solve(S)

b <- rnorm(n)
C <- diag(x = exp(rnorm(n)), n, n)

b.save <- b
C.save <- C

QQ <- Q + C
bb <- Q %*% mu + b

mu.star <- solve(QQ, bb)
S.star <- solve(QQ)

## remove data 
idx <- 2:(n-1)
b[idx] <- 0
diag(C)[idx] <- 0

QQ.remove <- Q + C
bb.remove <- Q %*% mu + b

S.remove <- solve(QQ.remove)
mu.remove <- S.remove %*% bb.remove

## compute the same thing correcting


C.correct <- C.save
C.correct[1, 1] <- C.correct[n, n] <- 0
b.correct <- b.save
b.correct[c(1, n)] <- 0

QQ.correct <- QQ - C.correct
bb.correct <- bb - b.correct
S.correct <- solve(QQ.correct)
mu.correct <- S.correct %*% bb.correct

print(S.correct - S.remove)
print(mu.correct - mu.remove)

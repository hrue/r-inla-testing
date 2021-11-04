n <- 100
cidx <- seq(1, n, by = 10)
n.cidx <- length(cidx)
val <- sin(cidx) + 0.5*cidx

idx <- setdiff(1:n, cidx)
Q <- INLA:::inla.rw(n, order = 2, sparse = FALSE)[c(idx, cidx), c(idx, cidx)]

m1 <- length(idx)
m2 <- length(cidx)
Q11 <- Q[1:m1, 1:m1]
Q12 <- Q[1:m1, m1 + 1:m2]

for(i in 1:1000) {
    mean <- solve(Q11, -Q12 %*% val)
    mu <- rep(0, n)
    mu[idx] <- mean
    mu[cidx] <- val
    plot(1:n, mu, pch = 19, cex = 5, col = "red")
    points(cidx, mu[cidx], pch = 19, col = "blue", cex = 7)
    val <- val + rnorm(n.cidx, sd = 0.3)
}

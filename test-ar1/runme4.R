library(mvtnorm)
n <- 1000
rho <- c(0.9, 0.7, 0.5)

x1 <- numeric(n)
x2 <- numeric(n)
x1[1] <- rnorm(1)
x2[2] <- rnorm(1)
for(i in 2:n) {
    v <- rmvnorm(1, sigma = matrix(c(1, rho[2], rho[2], 1), 2, 2))
    x1[i] <- rho[1] * x1[i-1] + v[1]
    x2[i] <- rho[1] * x2[i-1] + v[2]
}
x1 <- scale(x1)
x2 <- scale(x2)
y1 <- numeric(n)
y2 <- numeric(n)
for(i in 1:n) {
    v <- rmvnorm(1, sigma = matrix(c(1, rho[3], rho[3], 1), 2, 2))
    y1[i] <- x1[i] + 0.05 * v[1]
    y2[i] <- x2[i] + 0.05 * v[2]
}


r <- inla(
    y ~ -1 + f(idx, model = "ar1", group = g) +
        f(iidx, model = "iid2d", n = 2*n),
    data = data.frame(y = c(y1, y2),
                      idx = c(1:n, 1:n),
                      g = rep(1:2, each = n),
                      iidx = 1:(2*n)),
    family = "gaussian",
    control.family = list(hyper = list(
                              prec = list(initial = 12,
                                          fixed = TRUE))))

robeta <- function(eta, k1, k2, precision) {
    stopifnot(k1 <= k2)
    g <- function(...) inla.link.invlogit(...)

    p <- cbind(1 - g(eta - k1), g(eta - k1) - g(eta - k2), g(eta - k2))
    eta <- as.vector(eta)
    n <- length(eta)
    group <- numeric(n)
    x <- numeric(n)
    for(i in 1:n) {
        group[i] <- sample(1:3, 1, prob = p[i, ])
        if (group[i] == 1) {
            x[i] <- 0
        } else if (group[i] == 3) {
            x[i] <- 1
        } else {
            mu <- g(eta[i])
            a <- mu * precision
            b <- -mu * precision + precision
            x[i] <- rbeta(1, a, b)
        }
    }
    return (x)
}

n <- 100
eta <- rnorm(n)
x <- robeta(eta, -2, 2, 2)
print(c(sum(x == 0), sum(x == 1)))

x <- robeta(eta, -20, 20, 2)
print(c(sum(x == 0), sum(x == 1)))



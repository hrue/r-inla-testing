##    logll[i] = log(prob + (1.0 - prob) / (1+exp(-x)))

fix <- function(x, p) {
    ll <- function(x, p) log(p + (1-p) / (1+exp(-x)))

    x0 <- 0.5 * log(p)
    xL <- x0 + 0.01
    xH <- xL + 0.20

    idx <- which(x <= xH)
    res <- numeric(length(x))
    res[-idx] <- ll(x[-idx], p)

    if (length(idx) > 0) {
        z <- xH - x[idx]
        xx <- xH + (xL - xH) * z / (1 + z)
        zz <- x[idx] - xx
        c0 <- exp(-xx)
        c1 <- 1/(1 + c0)
        c2 <- 1/(p * c0 + 1)
        c3 <- c1 * c2
        g0 <- log(p + (1-p) * c1)
        g1 <- (1 - p) * c0 * c3
        g2 <- g1 * (p * c0^2 - 1) * c3
        res[idx] <- g0 + zz * (g1 + 0.5 * g2 * zz)
    }
    return (res)
}

p <- 0.1
lambda <- function(x) exp(x)
ll <- function(x, p) log(p + (1-p) / (1+exp(-x)))
ll_1m <- function(x, p) log(1-(p + (1-p) / (1+exp(-x))))
xx <- seq(-5, 5, by = 0.001)
yy <- ll(xx, p)
yy.fix <- fix(xx, p)

fun <- splinefun(xx, yy)
fun.fix <- splinefun(xx, yy.fix)

par(mfrow = c(1, 3))
plot(xx, fun(xx), type = "l")
lines(xx, fun.fix(xx), lty = 2)
abline(v = 0.5 * log(p))

plot(xx, fun.fix(xx, deriv = 1), type = "l")
lines(xx, fun(xx, deriv = 1), type = "l")
abline(v = 0.5 * log(p))
plot(xx, fun(xx, deriv = 2), type = "l")
lines(xx, fun.fix(xx, deriv = 2), type = "l")
abline(v = 0.5 * log(p))

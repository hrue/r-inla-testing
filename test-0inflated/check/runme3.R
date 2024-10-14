## p = prob + (1.0 - prob) / (1+exp(-x))

fix <- function(x, p, y, ny, true = FALSE)
{
    l1 <- function(xx, p) log(p + (1-p) / (1+exp(-xx)))
    l2 <- function(xx, p) log(1-(p + (1-p) / (1+exp(-xx))))
    ll <- function(xx, p) y * l1(xx, p) + ny * l2(xx, p)

    if (true) {
        return (ll(x, p))
    }

    n <- y + ny
    arg <- -(ny * p + (1-p) * sqrt(y * p * n )) / (p * (n * p - y))
    if (arg > 0) {
        x0 <- -log(-(ny * p + (1-p) * sqrt(y * p * n )) / (p * (n * p - y)))
    } else {
        x0 <- -Inf
    }

    xL <- x0 + 0.01
    xH <- x0 + 0.25
    idx <- which(x < xH)
    res <- numeric(length(x))
    if (length(idx) > 0) {
        res[-idx] <- ll(x[-idx], p)
    } else {
        res <- ll(x, p)
    }

    if (length(idx) > 0) {
        z <- xH - x[idx]
        xx <- xH + (xL - xH) * z / (1 + z)
        zz <- x[idx] - xx

        emx <- exp(-xx)
        em2x <- emx^2
        c1 <- 1 / (1 + emx)
        c2 <- 1 / (1 + p * emx)
        g0 <- ll(xx, p)
        g1 <- - c1 * c2 * (emx * (-y + n*p) + ny)
        g2 <- - c1^2 * c2^2 * emx * ((n * em2x * p + 2 * ny * emx - y * em2x - y) * p + n)
        res[idx] <- g0 + zz * (g1 + 0.5 * g2 * zz)
    }
    return (res)
}

p <- 0.16
y <- 1
ny <- 7

x <- seq(-4, 4, by = 0.01)
ll <- fix(x, p, y, ny, true = TRUE)
fun <- splinefun(x, ll)

ll.new <- fix(x, p, y, ny, true = FALSE)
fun.new <- splinefun(x, ll.new)

par(mfrow = c(1, 2))
plot(x, fun(x),  type = "l")
lines(x, fun.new(x),  lty = 2)

plot(x, fun(x, deriv = 2),  type = "l")
lines(x, fun.new(x, deriv = 2), lty = 2)

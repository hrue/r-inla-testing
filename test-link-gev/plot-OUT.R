x <- read.table("OUT-positive")[, c(3, 6, 7, 10, 11)]
y <- read.table("OUT-negative")[, c(3, 6, 7, 10, 11)]
z <- read.table("OUT-zero")[, c(3, 6, 7, 10, 11)]

plot(z[, 1], z[, 4], type = "l", lty = 1, lwd = 2)
lines(x[, 1], x[, 4], type = "l", lty = 2, lwd = 2)
lines(y[, 1], y[, 4], type = "l", lty = 2, lwd = 4)

inla.dev.new()
plot(z[, 1], z[, 5], type = "l", lty = 1, lwd = 2)
lines(x[, 1], x[, 5], type = "l", lty = 2, lwd = 2)
lines(y[, 1], y[, 5], type = "l", lty = 2, lwd = 3)


xy <- read.table("points.txt")
xxyy <- read.table("spline.txt")

plot(xxyy[, 1], xxyy[, 2], type = "l", lwd = 5, col = "red")
points(xy[, 1], xy[, 2], pch = 19)

fun <- splinefun(xy[, 1], xy[, 2], method = "natural")
x <- xxyy[, 1]
lines(x, fun(x), col = "yellow", lwd = 2)

mean(abs(fun(x) - xxyy[, 2]))




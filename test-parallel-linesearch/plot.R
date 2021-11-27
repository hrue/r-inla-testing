a <- read.table("res", col.names = c("type", "nfun", "cpu", "fval"))

plot(density(a[a$type == 2, "cpu"]), lwd = 3, col = "blue", xlim = c(10, 90))
lines(density(a[a$type == 1, "cpu"]), lwd = 3, col = "red")

if (TRUE) {
    dev.new()
    plot(density(a[a$type == 2, "nfun"]), lwd = 3, col = "blue", xlim = c(100, 10000))
    lines(density(a[a$type == 1, "nfun"]), lwd = 3, col = "red")
}

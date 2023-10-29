n = 30
values = 1:n
x <- c(4, 8, 16)
y = c(4, 0, 16)

r = inla(y ~ -1 + f(x, model="rw2", values = values,
                    constr = T, 
                    scale.model = TRUE,
                    hyper = list(prec = list(
                                     prior = "pc.prec",
                                     param = c(1, 0.01)))), 
        data = list(y = y, x = x, values = values),
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = 10,
                                fixed = TRUE))),
        verbose = TRUE)

plot(values, r$summary.random$x$mean, type = "l", ylim = c(-50, 20), lwd = 3)
lines(values, r$summary.random$x$"0.025quant", type = "l", lty = 3)
lines(values, r$summary.random$x$"0.975quant", type = "l", lty = 3)
points(x, y, pch = 19, cex = 2)
points(values, r$summary.random$x$mean)



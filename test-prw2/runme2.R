INLA:::inla.my.update(b = T)

n <- 200
loc <- seq(0, n-1, length.out=n)
h.size <- diff(range(loc)) / (n-1)
x <- loc / n
f.true <- (sin(2*pi*x^2))^3
s = 1
y <- f.true + rnorm(n, sd = s)
plot(loc, y, pch = 19)
lines(loc, f.true, type='l', lwd = 3, col='blue')

r <- inla(y ~ -1 + f(loc, model = "prw2", values = loc,
                     hyper = list(
                         prec =  list(initial = log(5),
                                      fixed = TRUE), 
                         range = list(prior = "pc.prw2.range",
                                      param = c(50, 0.5, h.size, 0),
                                      initial = log(10)))), 
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))), 
          data = data.frame(y, loc),
          safe = FALSE, 
          verbose = TRUE)
lines(loc, r$summary.linear.predictor$mean, lwd = 3, col = "red")

fac <- 0.5
loc <- fac * loc
h.size <- h.size * fac
dev.new()
plot(loc, y, pch = 19)
lines(loc, f.true, type='l', lwd = 3, col='blue')

rr <- inla(y ~ -1 + f(loc, model = "prw2", values = loc,
                     hyper = list(
                         prec =  list(initial = log(5),
                                      fixed = TRUE), 
                         range = list(prior = "pc.prw2.range",
                                      param = c(fac * 50, 0.5, h.size, 0),
                                      initial = log(10)))), 
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))), 
          data = data.frame(y, loc),
          safe = FALSE, 
          verbose = TRUE)
lines(loc, r$summary.linear.predictor$mean, lwd = 3, col = "red")

r <- inla.rerun(r)
rr <- inla.rerun(rr)
m <- r$internal.marginals.hyperpar[[1]]
mm <- rr$internal.marginals.hyperpar[[1]]
mm[, 1] <- mm[, 1] - log(fac)
mm[, 2] <- mm[, 2]

plot(inla.smarginal(m), type = "l", lwd = 3)
lines(inla.smarginal(mm), lwd = 3)



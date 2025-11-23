##INLA:::inla.my.update(b = T)

n <- 200
loc <- seq(0, n-1, length.out=n)
h.size <- diff(range(loc)) / (n-1)
x <- loc / n
f.true <- (sin(2*pi*x^2))^3
s = 0.4
y <- f.true + rnorm(n, sd = s)
plot(loc, y, pch = 19)
lines(loc, f.true, type='l', lwd = 1, lty = 3)

r <- inla(y ~ -1 + f(loc, model = "prw2", values = loc,
                     hyper = list(prec = list(initial = log(5),
                                              fixed = !TRUE), 
                                  range = list(param = c(70, 0.5, h.size, 0),
                                               initial = log(40),
                                               fixed = !TRUE))), 
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          data = data.frame(y, loc),
          safe = FALSE, 
          verbose = TRUE)

w <- 1/sqrt(diag(INLA:::inla.ginv(INLA:::inla.rw2(n, scale.model = TRUE), rankdef = 2)))
rr <- inla(y ~ -1 + f(loc, w, model = "rw2", values = loc, scale.model = TRUE, constr = FALSE, 
                      hyper = list(prec =  list(prior = "pc.prec",
                                                param = c(1, 0.01), 
                                                initial = log(5),
                                                fixed = !TRUE))), 
           family = "normal",
           control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                          fixed = TRUE))), 
           data = data.frame(y, loc, w),
           safe = FALSE, 
           verbose = TRUE)
lines(loc, r$summary.linear.predictor$mean, lwd = 3, col = "red")
lines(loc, rr$summary.linear.predictor$mean, lwd = 3, col = "blue")

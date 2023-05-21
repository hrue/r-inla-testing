## Use the spline part of scopy to estimate a spline.
## This example is rather artifical, but illustrate
## the idea.

N <- 100
s <- 0.1
x <- 1:N
y <- 1 + sin(x * 0.25) * exp(-2*x/N) + rnorm(N, sd = s)
m <- 15

r <- inla(y ~ -1 +
              f(one,
                model = "iid",
                values = 1, 
                hyper = list(prec = list(initial = 0, fixed = TRUE))) +
              ##
              ## the 'overall level' is scaled by a spline
              ##
              f(one.scopy,
                scopy = "one",
                control.scopy = list(covariate = x,
                                     n = m,
                                     mean = 1,
                                     prec.mean = 1,
                                     prec.betas = 10,
                                     model = "rw2")),
          ##
          data = list(one = rep(NA, N), 
                      one.scopy = rep(1, N), 
                      x = x,
                      m = m),
          ##
          control.family = list(hyper = list(
                                    prec =  list(
                                        initial = log(1/s^2),
                                        fixed = TRUE))),
          ##
          control.inla = list(int.strategy = "eb"), 
          verbose = TRUE)

plot(x, y, pch = 19)

xr <- range(x)
loc <- seq(xr[1], xr[2], len = m)
fun <- splinefun(loc,
                 r$summary.hyperpar[, "mean"] *
                 mean(r$summary.random$one$mean), 
                 method = "natural")
lines(x, fun(x), lwd = 3, col = "blue")

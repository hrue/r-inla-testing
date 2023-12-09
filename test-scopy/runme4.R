INLA:::inla.my.update(b = TRUE)
inla.setOption(smtp = 'taucs')
inla.setOption(safe = FALSE)
inla.setOption(num.threads = "1:1")

N <- 50
s <- 0.2
x <- 1:N
eta <- 1 + 0. * x + sin(x * 0.25) * exp(-2*x/N) 
y <- eta + rnorm(N, sd = s)
m <- 15

r <- inla(y ~ -1 +
              ## this model will just define the 'overall level', but
              ## with one value for each i. We need this as as can then
              ## scale this one with the spline
              f(idx,
                model = "rw1",
                scale.model = TRUE,
                constr = FALSE,
                values = 1:N,
                hyper = list(prec = list(initial = 20, fixed = TRUE))) +
              ## the 'overall level' is scaled by a spline
              f(idx.scopy, scopy = "idx",
                hyper = list(mean = list(param = c(1, 0.1)),
                             slope = list(param = c(0, 0.1)),
                             spline = list(param = c(0, 0.1))), 
                control.scopy = list(covariate = x, n = m)), 
          ##
          data = list(idx = rep(NA, N), 
                      idx.scopy = 1:N,
                      x = x,
                      m = m),
          ##
          ##control.inla = list(int.strategy = "eb"), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(
                                                 initial = log(1/s^2),
                                                 fixed = TRUE))),
          control.compute = list(config = TRUE, residuals = TRUE))

r <- inla.rerun(r)

plot(x, y, pch = 19)
## note that the locations are not stored in the results, hence we can set them here. This is
## just for the ease of the output, the results are unchanged. 
beta <- inla.summary.scopy(r, "idx.scopy", range = c(1, N))
s.mean <- mean(r$summary.random$idx$mean)
lines(beta$x, s.mean * beta$mean, lwd = 3, col = "blue")
lines(beta$x, s.mean * (beta$mean + 2 * beta$sd), lwd = 2, lty = 2, col = "black")
lines(beta$x, s.mean * (beta$mean - 2 * beta$sd), lwd = 2, lty = 2, col = "black")

lines(1:N, r$summary.linear.predictor$mean, lwd = 5, col = "red")
lines(1:N, r$summary.linear.predictor$'0.025quant', lwd = 5, lty = 2, col = "red")
lines(1:N, r$summary.linear.predictor$'0.975quant', lwd = 5, lty = 2, col = "red")

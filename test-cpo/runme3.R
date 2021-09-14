n <- 50
x <- arima.sim(n, model = list(ar = 0.9))
x <- 0.2 * as.numeric(scale(x))
intercept <- 2
eta <- intercept + x
y <- rpois(n, lambda = exp(eta))
r <- inla(y ~ 1 + f(time, model = 'ar1'), 
          data = data.frame(y, time = 1:n),
          family = "poisson",
          control.compute = list(cpo = TRUE), 
          ##verbose = TRUE,
          inla.call = 'inla.mkl.work')


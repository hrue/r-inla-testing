n = 1000L
p = 3L
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
x = arima.sim(n, model = list(ar = phi))
x = x/sd(x)
y = x + rnorm(n, sd=1/100.0)
idx = 1L:n

## (U, alpha)
param.prec = c(1, 0.01)
lambda = 3

r = (inla(y ~ -1 +
          f(idx, model='ar',
            order = p, 
            hyper = list(
                ## marginal precision
                prec = list(param = param.prec), 
                ## the parameters for the joint normal prior for the
                ## transformed pacf's, goes here.
                pacf1 = list(param = lambda))), 
          family = "gaussian",
          verbose=TRUE,
          data = data.frame(y, idx)))

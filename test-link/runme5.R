n = 100
idx = 1:n
x = c(scale(arima.sim(n,  model = list(ar = 0.9))))
y = rpois(n, lambda = exp(2+x))
y[(n-20):n] = NA

r = inla(y ~ 1 + f(idx, model="ar1"),
         data = data.frame(y, idx),
         family = "poisson",
         control.family = list(
             control.link = list(model = "quantile", quantile = 0.9)),
         verbose = TRUE,
         control.predictor = list(compute = TRUE, A = diag(n)))


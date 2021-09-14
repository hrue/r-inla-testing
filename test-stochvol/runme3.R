n = 3000
m = 30
N = n + m
x = arima.sim(n, model= list(ar = 0.98))
x = x/sd(x)
y = rnorm(n, sd = sqrt(exp(x)))
Y = c(y, rep(NA, m))
r = inla(Y ~ 1 + f(idx, model="ar1"), 
        data = data.frame(Y, idx=1:N),
        family = "stochvol",
        verbose=TRUE, 
        control.predictor = list(compute = TRUE, link = 1))

plot(1:n, r$summary.random$idx$mean[1:n], xlim = c(1, N),
     type = "l", ylim = c(-3, 3))
lines((n+1):N, r$summary.random$idx$mean[(n+1):N],
      lwd = 5)
lines((n+1):N, r$summary.random$idx$"0.025quant"[(n+1):N],
      lwd = 3)
lines((n+1):N, r$summary.random$idx$"0.975quant"[(n+1):N],
      lwd = 3)





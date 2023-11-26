n = 300
rho <- 0.9
x = arima.sim(n, model=list(ar=rho))
x = x/sd(x)
s <- 0.1
y = 1 + x + rnorm(n, sd = s)
r = inla(y ~ 1 + f(idx, model="ar1"),
    data = data.frame(y, idx=1:n),
    family = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))
summary(r)

## fix the correlation to 'rho', this needs to be converted to the internal paramerization
inla.models()$latent$ar1$hyper$theta2$to.theta
rho.intern <- inla.models()$latent$ar1$hyper$theta2$to.theta(rho)
rr = inla(y ~ 1 + f(idx, model="ar1",
                    hyper = list(rho = list(initial = rho.intern,
                                            fixed = TRUE))), 
    data = data.frame(y, idx=1:n),
    family = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

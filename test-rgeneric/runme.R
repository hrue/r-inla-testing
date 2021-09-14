n = 100
rho=0.9
y = arima.sim(n, model = list(ar = rho)) * sqrt(1-rho^2)
idx = 1:n
model = inla.rgeneric.define(inla.rgeneric.ar1.model, n=n, ntheta = 2, debug=T)
formula = y ~ -1 + f(idx, model="rgeneric", rgeneric = model)
r = inla(formula, data = data.frame(y, idx), family = "gaussian")

fformula = y ~ -1 + f(idx, model="ar1",
    hyper = list(
        prec = list(
            prior = "loggamma",
            param = c(1, 1)),
        rho = list(
            prior = "gaussian",
            param = c(0, 1))))
rr = inla(fformula, data = data.frame(y, idx), family = "gaussian")

inla.dev.new()
plot(rr$internal.marginals.hyperpar[[2]])
lines(rr$internal.marginals.hyperpar[[2]])
inla.dev.new()
plot(rr$internal.marginals.hyperpar[[3]])
lines(rr$internal.marginals.hyperpar[[3]])

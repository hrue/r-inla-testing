n = 100
rho = 0.9
x = arima.sim(n=n, model = list(ar = rho))
x = x - mean(x)
z = rnorm(n)
y = 1 + z + x + rnorm(n, sd=0.01)

idx = 1:n
formula = y ~ 1 + z + f(idx, model="ar1", diagonal = 0, constr=TRUE)
r = inla(formula,  data = data.frame(y, z, idx),
        control.compute = list(config = TRUE))
a=r$misc$configs$config[[1]]

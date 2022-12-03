n = 100
rho=0.9
x = arima.sim(n, model = list(ar = rho)) * sqrt(1-rho^2)
y = x + rnorm(n, sd = 0.1)
model = inla.cgeneric.define(
    model = "inla_cgeneric_ar1_model",
    shlib = "libdemo.so", n = n)    


formula = y ~ -1 + f(idx, model=model)
r = inla(formula,
         data = data.frame(y, idx = 1:n),
         verbose = TRUE,
         keep = TRUE,
         inla.call = "inla.mkl.work",
         num.threads = "4:2")



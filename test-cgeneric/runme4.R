inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")

system("make -B")
n <- 5
x <- arima.sim(n, model = list(ar = 0.9))
x <- scale(x)
s <- 0.01
y <- x + rnorm(n, sd = s)

## not entirely the same due to the boundary conditions, but almost as we scale 'y'
r <- inla(
    y ~ -1 + f(idx, model = "ar1",
              hyper = list(prec = list(prior = "loggamma",
                                       param = c(1, 1),
                                       initial = 1),
                           rho = list(prior = "normal",
                                      param = c(0, 1),
                                      initial = 1))), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

cmodel <- inla.cgeneric.define(model = "inla_cgeneric_ar1_model", shlib = "cgeneric-demo.so",
                               n = n, debug = !FALSE)
rc <- inla(
    y ~ -1 + f(idx, model = cmodel), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
    verbose = TRUE)


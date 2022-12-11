library(INLA)
## inla.setOption(inla.call = "inla.mkl.work")

n <- 50
system("./compile" )
cmodel <- inla.cgeneric.define(model = "inla_cgeneric_iid_model",
                               shlib = "cgeneric-demo.so",
                               n = n, debug = !TRUE)
y <- rnorm(n)
s <- 0.01

inla.cgeneric.q(cmodel)

rc <- inla(y ~ -1 + f(idx, model = cmodel),
           num.threads = "2:2",
           data = data.frame(y, idx = 1:n),
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
           safe = FALSE, 
           verbose = TRUE)

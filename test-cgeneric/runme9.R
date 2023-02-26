INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")



system("make -B")
n <- 5
cmodel <- inla.cgeneric.define(model = "inla_cgeneric_iid_model",
                               shlib = "cgeneric-demo.so",
                               n = n,
                               debug = TRUE, 
                               ii = as.integer(1:10),
                               jj = as.integer(1:10), 
                               x = as.double(1:10),
                               xx = as.double(1:10))

y <- rnorm(n)
r <- inla(
    y ~ 1 + f(idx, model = cmodel), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed = TRUE))),
    verbose = TRUE)

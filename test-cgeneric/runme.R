INLA:::inla.my.update(b = T)
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "4:1")

system("make -B")
n <- 1000
y <- rnorm(n)

r <- inla(
    y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(param = c(1, 1)))), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed = TRUE))))

rmodel <- inla.rgeneric.define(inla.rgeneric.iid.model, n = n)
rr <- inla(
    y ~ -1 + f(idx, model = rmodel), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed = TRUE))),
    verbose = TRUE)

cmodel <- inla.cgeneric.define(model = "inla_cgeneric_iid_model", shlib = "cgeneric-demo.so", n = n, debug = FALSE)
rc <- inla(
    y ~ -1 + f(idx, model = cmodel), 
    data = data.frame(y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed = TRUE))),
    verbose = TRUE)


print(cbind(r$mlik, rr$mlik-r$mlik, rc$mlik-r$mlik))
print(cbind(r$cpu[2], rr$cpu[2], rc$cpu[2]))

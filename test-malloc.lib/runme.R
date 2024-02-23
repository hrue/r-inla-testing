data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

res <- c()
for (lib in c("default", "je", "tc", "mi")) {
    inla.setOption(malloc.lib = lib)
    U = 0.3/0.31
    alpha = 0.01
    pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))
    formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
        f(inla.group(wbc), model="rw2",
          scale.model = TRUE, hyper = pc.param)+
        f(inla.group(tpi), model="rw2",
          scale.model = TRUE, hyper = pc.param)+
        f(district,model="besag",graph = g,
          scale.model = TRUE, hyper = pc.param)

    result = inla(formula, family="coxph", data=Leuk,
                  control.inla = list(int.strategy = "eb"),
                  control.hazard = list(scale.model = TRUE, hyper = pc.param),
                  verbose = T, debug = T)

    res <- rbind(res, c(lib, result$cpu))
}
print(res)

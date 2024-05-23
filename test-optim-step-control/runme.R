n <- 1000
s <- 10
y <- rnorm(n, sd = s)

inla.setOption(verbose = TRUE,
               num.threads = 1,
               inla.call = "inla.mkl.work")
r <- inla(y ~ 1, data = data.frame(y),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 4))))
plot(r,  plot.opt.trace = TRUE)
r$misc$nfunc

if (F) {
    data(Leuk)
    source(system.file("demodata/Leuk-map.R", package="INLA"))
    g = system.file("demodata/Leuk.graph", package="INLA")

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

    r = inla(formula, family="coxph", data=Leuk,
             control.hazard = list(scale.model = TRUE, hyper = pc.param, n.intervals = 100),
             control.inla = list(control.vb = list(enable = FALSE)))
    plot(r,  plot.opt.trace = TRUE)
    r$misc$nfunc
}

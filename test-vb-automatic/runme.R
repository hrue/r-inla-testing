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

result = inla(formula, family="coxph", data=Leuk,
              control.hazard = list(scale.model = TRUE, hyper = pc.param),
              verbose = TRUE,
              control.inla = list(int.strategy = "eb",
                                  control.vb = list(f.enable.limit = rep(9999, 4))), 
              inla.call = "inla.mkl.work")

data(Leuk)
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")
inla.setOption(smtp = "taucs")

U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", fixed = TRUE, 
      scale.model = TRUE, hyper = pc.param)+
    f(inla.group(tpi), model="rw2", fixed = TRUE, 
      scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g,
      scale.model = TRUE, hyper = pc.param)

result = inla(formula, family="coxph", data=Leuk,
              control.hazard = list(scale.model = TRUE,
                                    n.intervals = 10, 
                                    hyper = pc.param),
              control.compute = list(config = TRUE,
                                     cpo = T, 
                                     control.gcpo = list(
                                         enable = TRUE,
                                         num.level.sets = 25)), 
              keep = T,
              verbose = T, 
              num.threads = "4:2", 
              inla.mode = "compact",
              inla.call = "inla.mkl.work")



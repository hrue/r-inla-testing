data(Leuk)
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc, 50), model="rw2",
      scale.model = TRUE, hyper = pc.param)+
    f(inla.group(tpi, 50), model="rw2",
      scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g,
      scale.model = TRUE, hyper = pc.param)

r = inla(formula,
         family="coxph",
         data=Leuk,
         control.hazard = list(n.intervals = 100,
                               scale.model = TRUE,
                               hyper = pc.param),
         control.compute = list(config = TRUE, q = T, graph = T), 
         control.inla = list(h = 0.0000001), 
         inla.call = "inla.mkl.work",
         num.threads = "4:1", 
         verbose = TRUE)

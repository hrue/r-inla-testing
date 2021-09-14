INLA:::inla.my.update()
INLA:::inla.my.update()

data(Leuk)
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- rbind(Leuk, Leuk, Leuk, Leuk)

U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", diagonal = 1e-3, 
      scale.model = TRUE, hyper = pc.param, compute = FALSE)+
    f(inla.group(tpi), model="rw2", diagonal = 1e-3, 
      scale.model = TRUE, hyper = pc.param, compute = FALSE)+
    f(district, model="besag",graph = g, diagonal = 1e-3, 
      scale.model = TRUE, hyper = pc.param)

r = inla(formula,
         family="coxph",
         data=Leuk,
         control.hazard = list(n.intervals = 50,
                               scale.model = TRUE,
                               hyper = pc.param),
         inla.call = "inla.mkl.work", 
         inla.arg =  "-v -t4:1 -b", 
         num.threads = "4:1", 
         verbose = TRUE)

rr = inla(formula,
         family="coxph",
         data=Leuk,
         control.hazard = list(n.intervals = 50,
                               scale.model = TRUE,
                               hyper = pc.param),
         inla.call = "inla.mkl.work", 
         inla.arg =  "-v -t4:1 -b -P", 
         num.threads = "4:1", 
         verbose = TRUE)


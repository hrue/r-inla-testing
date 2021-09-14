data(Leuk)
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

Leuk <- rbind(Leuk, Leuk, Leuk, Leuk)

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", diagonal = 1e-2, 
      scale.model = TRUE, hyper = pc.param)+
    f(inla.group(tpi), model="rw2", diagonal = 1e-2, 
      scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g, diagonal = 1e-2, 
      scale.model = TRUE, hyper = pc.param)


r = inla(formula, family="coxph", data=Leuk,
         verbose = TRUE, 
         control.predictor = list(hyper = list(prec = list(initial = 13))), 
         control.fixed = list(prec = 1, prec.intercept = 1), 
         control.hazard = list(
             diagonal = 1e-2, 
             scale.model = TRUE,
             hyper = pc.param,
             n.intervals = 50))
         

rr = inla(formula, family="coxph", data=Leuk,
         verbose = TRUE, 
         control.predictor = list(hyper = list(prec = list(initial = 13))), 
         control.fixed = list(prec = 1, prec.intercept = 1), 
         control.inla = list(h = 1e-3), 
         control.hazard = list(
             diagonal = 1e-2, 
             scale.model = TRUE,
             hyper = pc.param,
             n.intervals = 50),
         twostage = TRUE)

r$mlik - rr$mlik

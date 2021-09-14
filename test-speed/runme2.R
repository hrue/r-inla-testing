data(Leuk)
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- rbind(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)

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
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.hazard = list(n.intervals = 50,
                               scale.model = TRUE,
                               hyper = pc.param),
         inla.call = "inla.mkl.work", 
         twostage = FALSE, 
         num.threads = "4:1", 
         ##control.mode = list(theta = c(-0.988, -1.107, -1.128, -1.108),
         ##                    restart = FALSE), 
         verbose = TRUE)


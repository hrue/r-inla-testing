data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")


Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ 1 + 
    f(district,model="besag",graph = g)


param <- rep(NA, 32)
##param[9] <- 1 ## use omp
r = inla(formula, family="gammasurv", data=Leuk,
         control.hazard = list(n.intervals = 50), 
         control.compute = list(dic = TRUE),
         control.stiles = list(param = param),
         keep = 1)

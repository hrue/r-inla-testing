data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")


Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ 1 + 
    f(district,model="besag",graph = g)

inla.setOption(inla.call = "inla.mkl.work")
r = inla(formula, family="gammasurv", data=Leuk,
          control.hazard = list(n.intervals = 50), 
           control.compute = list(dic = TRUE), 
          safe = F, 
          verbose = T, num.threads = "1:1")
rr = inla(formula, family="gammasurv", data=Leuk,
          control.hazard = list(n.intervals = 50), 
          inla.call = "inla.mkl.work", 
          control.compute = list(dic = TRUE), 
          safe = F, 
          verbose = T, num.threads = "1:1")

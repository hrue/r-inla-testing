data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)

inla.setOption(scale.model = T)

Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc,10), model="rw2")+
    f(inla.group(tpi,10), model="rw2")+
    f(district,model="besag",graph = g)

inla.setOption(scale.model = TRUE)

r = inla(formula, family="coxph", data=Leuk,
          control.hazard = list(n.intervals = 20), 
          control.inla = list(int.strategy = "eb"), 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     verbose = !TRUE, 
                                                     num.level.sets = 4)), 
          verbose = T,
          keep = !T, 
          safe = F,
          num.threads = "4:1")

rr = inla(formula, family="coxph", data=Leuk,
          control.hazard = list(n.intervals = 20), 
          control.inla = list(int.strategy = "eb"), 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     verbose = !TRUE, 
                                                     num.level.sets = 4)), 
          verbose = T,
          keep = T, 
          safe = F,
          num.threads = "4:1",
          inla.call = "inla.mkl.work")

plot(r$gcpo$gcpo,rr$gcpo$gcpo)

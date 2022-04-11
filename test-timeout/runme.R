data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)
##Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk)

Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)


inla.setOption(inla.timeout = 0)
inla.setOption(inla.call = "inla.mkl.work")

rr = inla(formula, family="coxph", data=Leuk,
          control.hazard = list(n.intervals = 50), 
          inla.mode = "experimental", verbose = T, num.threads = "4:1")

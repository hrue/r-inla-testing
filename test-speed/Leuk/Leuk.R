data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk)
Leuk <- inla.rbind.data.frames(Leuk, Leuk)
Leuk <- inla.rbind.data.frames(Leuk, Leuk)
Leuk <- inla.rbind.data.frames(Leuk, Leuk)

Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)

inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "4:1")
##inla.setOption(inla.call = "remote")
inla.setOption(inla.call = "inla.mkl.work")
##inla.setOption(inla.call = NULL)

##Sys.setenv(INLA_TRACE = 'Qx2')
##Sys.unsetenv('INLA_TRACE')


r = inla(formula, family="coxph", data=Leuk, safe = F, 
         control.hazard = list(n.intervals = 50))

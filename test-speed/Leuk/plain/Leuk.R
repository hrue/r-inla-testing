data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk)

Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", scale.model = TRUE)+
    f(inla.group(tpi), model="rw2", scale.model = TRUE)+
    f(district,model="besag",graph = g, scale.model = TRUE)

##Sys.setenv(INLA_TRACE = '*')

r = inla(formula, family="coxph", data=Leuk, keep = T, safe = F, 
         control.hazard = list(n.intervals = 50),
         verbose = F)

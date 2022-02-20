data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

inla.setOption(scale.model = TRUE)
Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)

r = inla(formula, family="weibullsurv", data=Leuk,
         inla.mode = "experimental",
         num.threads = "2:2",
         verbose = TRUE)

r$cpu
         


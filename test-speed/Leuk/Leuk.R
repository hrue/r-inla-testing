data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- rbind(Leuk, Leuk)
Leuk <- rbind(Leuk, Leuk)

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)

inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "2:4")
r = inla(formula, family="coxph", data=Leuk, twostage = FALSE, verbose = T)
rr = inla(formula, family="coxph", data=Leuk, twostage = TRUE, verbose = T)
rrr = inla(formula, family="coxph", data=Leuk, twostage = TRUE, verbose = T, keep = T, 
           control.inla = list(control.twostage = list(stage1only = TRUE)))
r$cpu
rr$cpu
rrr$cpu


data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

inla.setOption(scale.model = TRUE)
Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)

inla.setOption(num.threads = "8:1")
##inla.setOption(inla.call = "remote")
inla.setOption(inla.call = NULL)
r = inla(formula, family="coxph", data=Leuk)
rr = inla(formula, family="coxph", data=Leuk,
          inla.mode = "experimental")
print(round(dig = 2,
            c(classic = r$cpu[2], 
              experimental = rr$cpu[2], 
              ratio = r$cpu[2]/rr$cpu[2])))


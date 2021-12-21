data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)
##Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)

Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2")+
    f(inla.group(tpi), model="rw2")+
    f(district,model="besag",graph = g)

inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "2:1")
##inla.setOption(inla.call = "remote")
inla.setOption(inla.call = "inla.mkl.work")
##inla.setOption(inla.call = NULL)

##Sys.setenv(INLA_TRACE = 'Qx2')
##Sys.unsetenv('INLA_TRACE')

##r = inla(formula, family="coxph", data=Leuk)
rr = inla(formula, family="coxph", data=Leuk,
          ##control.inla = list(parallel.linesearch = TRUE), 
          inla.mode = "experimental", verbose = T, num.threads = "4:2")
print(rr$cpu[2])
##print(round(dig = 2, c(classic = r$cpu[2], experimental = rr$cpu[2], ratio = r$cpu[2]/rr$cpu[2])))

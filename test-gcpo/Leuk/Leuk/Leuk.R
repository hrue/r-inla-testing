data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

Leuk <- inla.rbind.data.frames(Leuk, Leuk, Leuk, Leuk, Leuk, Leuk)


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

##r = inla(formula, family="coxph", data=Leuk)
rr = inla(formula, family="coxph", data=Leuk,
          control.hazard = list(n.intervals = 10), 
          ##control.inla = list(parallel.linesearch = TRUE), 
          control.compute = list(cpo = T,
                                 control.gcpo = list(enable = TRUE,
                                                     group.size = 9)), 
          inla.mode = "experimental",
          verbose = T,
          keep = T, 
          num.threads = "4:1",
          inla.call = "inla.mkl.work")
print(rr$cpu[2])


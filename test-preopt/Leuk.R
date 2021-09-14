data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", scale.model = F)+
    f(inla.group(tpi), model="rw2", scale.model = F)+
    f(district, model="besag", graph = g, scale.model = F)

##inla.setOption(inla.call = NULL)
inla.setOption(inla.call = "inla.mkl.work")
result = inla(formula, family="coxph", data=Leuk,
              control.inla = list(verbose = F), 
              inla.arg = "-v -t4:1 -b -P", 
              keep = TRUE, 
              verbose = TRUE)
summary(result)

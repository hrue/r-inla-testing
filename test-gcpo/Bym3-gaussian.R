library(INLA)

INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g) + f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)

Germany$Y <- sqrt(Germany$Y / Germany$E)

r = inla(formula3,
         family="t",
         control.family = list(hyper = list(dof = list(initial = 2, fixed = TRUE),
                                            prec = list(initial = log(4), fixed = TRUE))), 
         data=Germany, 
         verbose = TRUE,
         safe = FALSE, 
         control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb", cmin = 0), 
         control.compute = list(
             config = TRUE, 
             cpo = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 num.level.sets = -1,
                 verbose = TRUE)))

source(system.file("demodata/Bym-map.R", package="INLA"))

plot(r$cpo$cpo, r$gcpo$gcpo)
abline(a=0,b=1)

inla.dev.new()
plot(r$cpo$pit, r$gcpo$pit)
abline(a=0,b=1)

inla.dev.new()
plot(cbind(r$misc$configs$config[[1]]$cpodens.moments[,1],
           r$misc$configs$config[[1]]$gcpodens.moments[,1]))
abline(a=0,b=1)

inla.dev.new()
plot(cbind(r$misc$configs$config[[1]]$cpodens.moments[,2],
           r$misc$configs$config[[1]]$gcpodens.moments[,2]))
abline(a=0,b=1)

inla.dev.new()
plot(cbind(r$misc$configs$config[[1]]$cpodens.moments[,3],
           r$misc$configs$config[[1]]$gcpodens.moments[,3]))
abline(a=0,b=1)

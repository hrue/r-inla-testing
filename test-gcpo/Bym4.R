library(INLA)

##INLA:::inla.my.update(b = T)
inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "1:1")
Sys.setenv(INLA_TRACE="GMRFLib_gcpo_build,GMRFLib_gcpo")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)
Germany = rbind(Germany, Germany)

formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g) +
    f(region,model="iid") + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         verbose = !TRUE,
         ##control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb"),
         ##control.vb = list(enable = FALSE)), 
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
               enable = TRUE, 
               num.level.sets = 8,
               verbose = !TRUE)))

##Sys.setenv(INLA_NEW_TEST = 1)
Sys.unsetenv("INLA_NEW_TEST")
rr = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         verbose = TRUE,
         ##control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb", cmin = -Inf),
         ##control.vb = list(enable = FALSE)), 
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
               enable = TRUE, 
               num.level.sets = 8,
               verbose = !TRUE)),
         safe = FALSE, 
         inla.call = "inla.mkl.work")

plot(r$gcpo$gcpo, rr$gcpo$gcpo); abline(a=0,b=1)
r$logfile[grep("gcpo", r$logfile)]
rr$logfile[grep("gcpo", rr$logfile)]


INLA:::inla.my.update(b = TRUE)
data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))


## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

inla.setOption(num.threads = "4:2",  smtp = 'taucs',
               verbose = FALSE, safe = FALSE)

r = inla(formula3,family="poisson",data=Germany,E=E,
         control.inla = list(int.strategy = "eb"))
theta <- r$mode$theta

formula3 = Y ~ f(region.struct,model="besag",graph=g, scale.model = TRUE,
                 initial = theta[1], fixed = T) +
    f(region,model="iid", initial = theta[2], fixed = T) +
    f(x, model="rw2", scale.model = TRUE, initial = theta[3], fixed = TRUE)

r = inla(formula3,family="poisson",data=Germany,E=E,
         control.compute = list(control.gcpo = list(enable = TRUE,
                                                 num.level.sets = 3)))
rr <- inla.group.cv(r, num.level.sets = 3)

inla.setOption(smtp = 'stiles')
v = inla(formula3,family="poisson",data=Germany,E=E,
         control.compute = list(control.gcpo = list(enable = TRUE,
                                                 num.level.sets = 3)))
vv <- inla.group.cv(v, num.level.sets = 3)
 
mean(abs(r$gcpo$gcpo - v$gcpo$gcpo))
mean(abs(rr$cv - vv$cv))
mean(abs(r$gcpo$gcpo - rr$cv))
mean(abs(v$gcpo$gcpo - vv$cv))



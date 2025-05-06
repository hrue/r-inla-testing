data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

## just make a duplicated column
formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

inla.setOption(num.threads = "1:1", safe = FALSE, verbose = T)
INLA:::inla.my.update(b = T)
rr <- inla(formula3,family="poisson",data=Germany,E=E,
           control.inla = list(int.strategy = "eb"), 
           control.compute = list(smtp = 'taucs'))

r <- inla(formula3,family="poisson",data=Germany,E=E,
          control.inla = list(int.strategy = "eb"), 
          control.compute = list(smtp = 'stiles'))

r$mlik - rr$mlik
max(abs(r$summary.linear.predictor$sd/rr$summary.linear.predictor$sd -1))
max(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean))

##cv <- inla.group.cv(r, num.level.set = 2, verbose = TRUE)

rr$logfile[grep("get(M", rr$logfile,fixed=TRUE)]
r$logfile[grep("get(M", r$logfile,fixed=TRUE)]

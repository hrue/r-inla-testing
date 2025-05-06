INLA:::inla.my.update(b = TRUE)
data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

# with smooth covariate
formula3 = Y ~ f(region.struct,model="besag",graph=g, scale.model = TRUE) +
               f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)

inla.setOption(num.threads = "4:4",  smtp = 'taucs')
r = inla(formula3,family="poisson",data=Germany,E=E,
         verbose = T)

inla.setOption(smtp = 'stiles')
rr = inla(formula3,family="poisson",data=Germany,E=E,
          verbose = T, keep = T)

max(abs(r$mlik - rr$mlik))
max(abs(r$summary.random$region.struct -rr$summary.random$region.struct))
max(abs(r$summary.linear.predictor -rr$summary.linear.predictor))


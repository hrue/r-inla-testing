data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

inla.setOption(scale.model.default = TRUE)
prior.iid = c(1,0.1)
prior.besag = c(1,0.001)

formula1 = Y ~ 1 + f(region.struct,
        model="besag",graph=g, constr=TRUE,
        hyper = list(prec = list(param = prior.besag))) +
    f(region,
      model="iid", hyper = list(prec = list(param = prior.iid)))
result1  =  inla(formula1,family="poisson",data=Germany,E=E,
        control.predictor = list(compute=TRUE))

formula1.bym = Y ~ 1 + f(region,
        model = "bym", graph = g, constr=TRUE, 
        hyper = list(
                prec.unstruct = list(param = prior.iid),
                prec.spatial = list(param = prior.besag)))

result1.bym = inla(formula1.bym,family="poisson",data=Germany,E=E, 
        control.predictor = list(compute=TRUE))

print(mean(abs(result1$summary.linear.predictor$mean-result1.bym$summary.linear.predictor$mean)))


par(mfrow=c(2, 2))

result1$internal.summary.hyperpar
result1.bym$internal.summary.hyperpar
plot(result1$internal.marginals.hyperpar[[1]])
lines(result1.bym$internal.marginals.hyperpar[[2]])
title("STD: log prec.spatial")
plot(result1$internal.marginals.hyperpar[[2]])
lines(result1.bym$internal.marginals.hyperpar[[1]])
title("STD: log prec.iid")

result1 = inla.hyperpar(result1, diff.logdens=10, dz=.5)
result1.bym = inla.hyperpar(result1.bym, diff.logdens=10, dz=.5)
summary(result1)
summary(result1.bym)
plot(result1$internal.marginals.hyperpar[[1]])
lines(result1.bym$internal.marginals.hyperpar[[2]])
title("IMPROVED: log prec.spatial")
plot(result1$internal.marginals.hyperpar[[2]])
lines(result1.bym$internal.marginals.hyperpar[[1]])
title("IMPROVED: log prec.iid")



inla.my.update(b=T)
assignInNamespace("f", f, "INLA")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))

Germany = cbind(Germany,region.struct=Germany$region)

g.matrix = inla.graph2matrix(inla.read.graph(g))

formula2 = Y ~ f(region.struct,model="besag",graph.file=g.matrix) +
               f(region,model="iid") + x

result2 =  inla(formula2,family="poisson",data=Germany,E=E, keep=TRUE, verbose=T)

Bym.map(result2$summary.random$region.struct$mean)

prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3

formula1.bym = Y ~ f(region, model = "bym", graph.file = g.matrix,
                     param = c(prior.iid, prior.besag),
                     initial = c(initial.iid, initial.besag))
result1.bym = inla(formula1.bym,family="poisson",data=Germany,E=E, keep=TRUE, verbose=T)



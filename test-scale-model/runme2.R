data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
graph = inla.read.graph(g)

A1 = list(
        A = rbind(rep(1, graph$n)),
        e = 0)

std=TRUE
Germany$Y[] = NA

r1 = inla(Y ~ -1 + f(region, model="besag", initial=0, fixed=TRUE, standardise=std,
        diagonal = 1e-9, graph = graph,
        constr = TRUE,  
        extraconstr=NULL),
        data = Germany, 
        verbose=FALSE)


exp(mean(log(r1$summary.random$region$sd^2)))



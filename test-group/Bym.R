data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))

Germany = cbind(Germany,region.struct=Germany$region)
Germany$x <- inla.group(Germany$x, n = 20)
formula3 = Y ~
    f(region.struct,model="besag",graph=g) +
    f(region,model="iid") +
    f(x, model="rw2", group = region.struct,
      control.group = list(model = "besag",  graph = g))

result3 =  inla(formula3,family="poisson",data=Germany,E=E,
                verbose = TRUE,
                control.inla = list(control.vb = list(enable = TRUE)))



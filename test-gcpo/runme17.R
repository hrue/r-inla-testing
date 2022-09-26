library(INLA)
inla.setOption(inla.mode = "classic")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

formula3 = Y ~ f(region, model="besag", graph=g) + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany,
         E=E,
         verbose = TRUE)

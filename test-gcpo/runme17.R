library(INLA)
INLA:::inla.my.update()
inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

formula3 = Y ~ f(region, model="besag", graph=g) + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany,
         control.compute = list(control.gcpo = list(enable = TRUE)), 
         safe = FALSE, 
         E=E,
         verbose = TRUE)

r1 <- inla.group.cv(r, num.level.sets = 1)
##r2 <- inla.group.cv(r, num.level.sets = 2)

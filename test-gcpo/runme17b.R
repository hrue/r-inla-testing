library(INLA)

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

Germany$id <- 1:nrow(Germany)
formula3 = Y ~ f(region, model="besag", graph=g) + f(id) + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany,
         safe = FALSE, 
         E=E,
         verbose = TRUE)

r1 <- inla.group.cv(r, min.overlap = 1, num.level.sets = 2)


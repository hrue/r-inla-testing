library(INLA)

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany$id <- 1:nrow(Germany)
formula3 = Y ~ f(region, model="besag", graph=g) + f(id) + f(x, model="rw2")

## just to get 'cpo' for comparison
r = inla(formula3, family="poisson", data=Germany, E=E,
         control.compute = list(cpo = TRUE))
cpo <- r$cpo$cpo


r = inla(formula3, family="poisson", data=Germany, E=E)
r <- inla.group.cv(r)

plot(cpo,  r$cv, log = "xy", pty = 19)
abline(a = 0, b = 1, lwd = 3)

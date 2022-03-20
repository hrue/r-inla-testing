library(INLA)

INLA:::inla.my.update(b = T)
inla.setOption(inla.mode = "experimental")
inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "4:1")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany,
         E=E,
         verbose = TRUE,
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 group.size = 1,
                 verbose = TRUE)))

source(system.file("demodata/Bym-map.R", package="INLA"))

n <- nrow(Germany)
m <- 3
par(mfrow = c(m, m))
for(ii in 240 + 0:8) {
    i <- ii+1
    if (ii > 9 && ii %% m^2 == 0) {
        dev.new()
        par(mfrow = c(m, m))
    }
    x <- rep(1, n)
    x[r$gcpo$groups[[i]]$idx] <- 0
    x[i] <- 2
    Bym.map(x)
}

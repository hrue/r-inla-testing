library(INLA)

INLA:::inla.my.update(b = T)
inla.setOption(inla.mode = "experimental")
inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "4:1")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g) + f(region,model="iid") + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         verbose = TRUE,
         safe = FALSE, 
         control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 group.size = 30,
                 group.size.max = 3,
                 strategy = "prior",
                 remove = c("region"), 
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

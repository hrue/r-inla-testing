library(INLA)
INLA:::inla.my.update(b = T)
inla.setOption(inla.mode = "experimental")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)
##Germany <- Germany[1:10, ]
##g <- inla.read.graph(matrix(1, 10, 10))

formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

r = inla(formula3,family="poisson",
         data=Germany,
         E=E,
         verbose = TRUE, 
         num.threads = "4:1", 
         ##control.inla = list(int.strategy = "eb"), 
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 group.size = 9,
                 verbose = !TRUE)))

source(system.file("demodata/Bym-map.R", package="INLA"))

n <- nrow(Germany)
m <- 3
par(mfrow = c(m, m))
for(ii in 0:100) {
    i <- ii+1
    if (ii > 9 && ii %% m^2 == 0) {
        dev.new()
        par(mfrow = c(m, m))
    }
    x <- rep(1, n)
    x[r$gcpo$groups[[i]]] <- 0
    x[i] <- 2
    Bym.map(x)
}

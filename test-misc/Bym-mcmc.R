library(INLA)
##inla.setOption(inla.mode = "experimental")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ f(region.struct,model="besag",graph=g, constr = T) +
               f(region,model="iid") + f(x, model="rw2")

tref <- Sys.time()
for(i in 1:10) {
    r =  inla(formula3,
              family="poisson",
              data=Germany,
              E=E,
              verbose = !TRUE, 
              num.threads = "1:1")
}
print(Sys.time() - tref)

tref <- Sys.time()
r.prev <- NULL
for(i in 1:10) {
    r =  inla(formula3,
              family="poisson",
              data=Germany,
              E=E,
              verbose = !TRUE, 
              num.threads = "1:1", 
              control.inla = list(int.strategy = "eb",
                                  optimise.strategy = if (is.null(r.prev)) "smart" else "plain", 
                                  control.vb = list(enable = FALSE)), 
              control.compute = list(return.marginals = FALSE),
              control.mode = list(result = r.prev, restart = TRUE))
    r.prev <- r
}
print(Sys.time() - tref)

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)
if (FALSE) {
    Germany = rbind(Germany,Germany)
    Germany = rbind(Germany,Germany)
    Germany = rbind(Germany,Germany)
}

formula = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

r <- inla(formula,
          family="poisson",
          data=Germany,
          E=E,
          verbose = !TRUE, 
          control.inla = list(
              int.strategy = "eb", 
              parallel.linesearch = TRUE))
rr <- inla(formula,
           family="poisson",
           data=Germany,
           E=E,
           verbose = !TRUE, 
           control.inla = list(
               int.strategy = "eb", 
               parallel.linesearch = !TRUE))
r$cpu.used
rr$cpu.used


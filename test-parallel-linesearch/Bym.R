data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

result3 =  inla(formula3,
                family="poisson",
                data=Germany,
                E=E,
                verbose = TRUE, 
                control.inla = list(
                    int.strategy = "eb", 
                    parallel.linesearch = TRUE),
                inla.mode = "experimental",
                inla.call = "inla.mkl.work",
                num.threads = "4:1")

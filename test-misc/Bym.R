library(INLA)
##inla.setOption(inla.mode = "experimental")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ f(region.struct,model="besag",graph=g, constr = T) +
               f(region,model="iid") + f(x, model="rw2")

result3 =  inla(formula3,family="poisson",
                data=Germany,
                E=E,
                verbose = TRUE, 
                num.threads = "1:1", 
                inla.call = "inla.mkl.work", 
                control.inla = list(int.strategy = "eb"))
summary(result3)
result3$mlik

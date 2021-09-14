data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
formula3 = Y ~ f(region.struct,model="besag",graph=g) +
    f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)
r = inla(formula3,family="poisson",data=Germany,E=E,
         control.inla = list(h = 0.005, strategy = "adaptive"), 
         inla.call = "inla.mkl.work", 
         verbose = TRUE,
         num.threads = "6:1")


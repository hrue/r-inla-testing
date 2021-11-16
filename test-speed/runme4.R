data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
formula3 = Y ~ f(region.struct,model="besag",graph=g) +
    f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)
r = inla(formula3,family="poisson",data=Germany,E=E,
         inla.call = "inla.mkl.work", 
         control.fixed = list(prec.intercept = 1), 
         verbose = TRUE,
         num.threads = "8:1")

rr = inla(formula3,family="poisson",data=Germany,E=E,
         control.inla = list(bfgs.version = 4), 
         control.fixed = list(prec.intercept = 1), 
         inla.call = "inla.mkl.work", 
         verbose = TRUE,
         num.threads = "8:1")


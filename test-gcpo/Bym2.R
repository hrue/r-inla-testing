library(INLA)

##INLA:::inla.my.update(b = T)
inla.setOption(inla.mode = "experimental")
inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)


formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g) +
    f(region,model="iid") + x
formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g,
                     diagonal = 1e-4, constr = F) +
    f(region,model="iid") + f(x, model="rw2", constr = F,
                              diagonal = 1e-4)

r = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         verbose = TRUE,
         ##control.fixed = list(prec.intercept = 1), 
         ##control.inla = list(int.strategy = "eb",
         ##control.vb = list(enable = FALSE)), 
         control.compute = list(
             cpo = T, 
             config = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 group.size = 2,
                 verbose = !TRUE)))

print(mean(log(r$gcpo$gcpo)))

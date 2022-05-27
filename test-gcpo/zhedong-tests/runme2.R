library(INLA)
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")
inla.setOption(scale.model = TRUE)
inla.setOption(smtp = "taucs")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

Sys.setenv(INLA_DEBUG = "gcpo")


formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g) + f(region,model="iid") + f(x, model="rw2")

r1 = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         safe = F, 
         verbose = TRUE,
         control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb", control.vb = list(enable = F)), 
         control.compute = list(
           control.gcpo = list(
             enable = TRUE, 
             selection = 93, 
             group.size = 2,
             verbose = TRUE)))

stop("XXXXXX")

r2 = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         safe = F, 
         verbose = TRUE,
         control.fixed = list(prec.intercept = 1), 
         control.inla = list(int.strategy = "eb", control.vb = list(enable = F)), 
         control.compute = list(
           control.gcpo = list(
             enable = TRUE, 
             selection = 93, 
             group.size = 5,
             verbose = TRUE)))

r1$gcpo$groups[[93]]
r2$gcpo$groups[[93]]

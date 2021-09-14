data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

# standard BYM model
formula1 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid")

result1  =  inla(formula1,family="poisson",data=Germany,E=E)
result1e  =  inla(formula1,family="poisson",data=Germany,E=E,
                  control.inla = list(control.vb = list(enable = TRUE)))

# with linear covariate
formula2 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + x

result2 =  inla(formula2,family="poisson",data=Germany,E=E)
result2e  =  inla(formula2,family="poisson",data=Germany,E=E,
                  control.inla = list(control.vb = list(enable = TRUE)))

# with smooth covariate
formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

result3 =  inla(formula3,family="poisson",data=Germany,E=E,
                control.predictor = list(compute = TRUE))
result3e  =  inla(formula3,family="poisson",data=Germany,E=E,
                  control.inla = list(control.vb = list(enable = TRUE, refinement = 2),
                                      strategy = "gaussian"))

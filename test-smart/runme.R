data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)
Germany = cbind(Germany,xx=Germany$x)
formula3 = Y ~ f(region.struct,model="besag",graph=g) +
    f(region,model="iid") + f(x, model="rw2") + f(xx, model = "rw1" )

r = inla(formula3,family="poisson",data=Germany,E=E,
         control.inla = list(optimise.strategy = "safe", h = 0.005),
         ##control.inla = list(optimise.strategy = "smart", h=0.005),
         verbose = TRUE)
print(r$misc$nfunc)


## SAFE
##StDev/Correlation matrix (scaled inverse Hessian)
##     0.240750     0.002383    -0.006949
##                  0.982073     0.000568
##                               0.553591
## SMART
##StDev/Correlation matrix (scaled inverse Hessian)
##     0.240750     0.002384    -0.006946
##                  0.982085     0.000572
##                               0.553592

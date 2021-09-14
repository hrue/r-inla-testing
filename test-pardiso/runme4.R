data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

formula1 = Y ~ 1 +
    f(region.struct,model="besag",graph=g, scale.model=TRUE, diagonal=1E-5) + 
    f(region,model="iid")

if (!exists("r.taucs")) {
    r.taucs = inla(formula1,family="poisson",data=Germany,E=E,
                   inla.call = INLA:::inla.call.builtin(),
                   control.compute = list(smtp = "taucs"))
}

INLA:::inla.my.update(b=T)
inla.setOption(pardiso.license = "/home/hrue/sys/licenses/pardiso.lic")

r.pardiso.s = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.serial"),
                   verbose=TRUE)

r.pardiso.p = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.parallel"),
                   verbose=TRUE)






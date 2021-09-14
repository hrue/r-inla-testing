data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

n = dim(Germany)[1]
nc = 4
constr = list(A = matrix(rnorm(nc*n), nc, n),  e = runif(nc))

formula1 = Y ~ 1 +
    f(region.struct,model="besag",graph=g, scale.model=TRUE) +
    f(region,model="iid", extraconstr = constr, diagonal=1)
##formula1 = Y ~ f(region,model="bym",graph=g)

if (!exists("r.taucs")) {
    r.taucs = inla(formula1,family="poisson",data=Germany,E=E,
                   inla.call = INLA:::inla.call.builtin(),
                   control.compute = list(smtp = "taucs"))
}

INLA:::inla.my.update(b=T)
inla.setOption(pardiso.license = "/home/hrue/sys/licenses/pardiso.lic")

r.pardiso.s = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.serial"),
                   verbose=TRUE, keep=TRUE, num.threads=2)

r.pardiso.p = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.parallel"),
                   keep=T, verbose=TRUE)






data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

K=6
for(k in 1:K)
    Germany = rbind(Germany,  Germany)
Germany$region = 1:nrow(Germany)
Germany$region.struct = 1:nrow(Germany)
G = inla.graph2matrix(g)
for(k in 1:K)
    G = cbind(rbind(G, G), rbind(G, G))
g = inla.read.graph(G)

formula1 = Y ~ 1 +
    f(region.struct,model="besag",graph=g, scale.model=TRUE, diagonal=1E-5) + 
    f(region,model="iid")

##INLA:::inla.my.update(b=T)
inla.setOption(pardiso.license = "/home/hrue/sys/licenses/pardiso.lic")

if (F) r.pardiso.s = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.serial"),
                   verbose=TRUE)

r.pardiso.p = inla(formula1,family="poisson",data=Germany,E=E,
                   control.compute = list(openmp.strategy = "pardiso.parallel"),
                   verbose=TRUE, num.threads = 8)






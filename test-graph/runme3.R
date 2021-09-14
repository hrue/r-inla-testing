library(maptools)
library(spdep)
library(INLA)


nc.sids <- readShapePoly(system.file("etc/shapes/sids.shp", package="spdep")[1])
g <- poly2nb(nc.sids)
n = length(g)

data(Germany)
Germany = cbind(Germany,region.struct=Germany$region)
Germany = Germany[1:n, ]

formula1 = Y ~ f(region.struct,model="besag",graph=g) +
                f(region,model="iid")

result1  =  inla(formula1,family="poisson",data=Germany,E=E)


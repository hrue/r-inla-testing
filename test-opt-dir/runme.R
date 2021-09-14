data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
Germany = cbind(Germany,region.struct=Germany$region)
Germany = rbind(Germany,Germany)
Germany = rbind(Germany,Germany)
Germany = rbind(Germany,Germany)

formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

h <- 0.005
r =  inla(formula3,family="poisson",data=Germany,E=E,
          verbose = T,
          control.inla = list(h = h))
rr =  inla(formula3,family="poisson",data=Germany,E=E,
          verbose = T,
          control.inla = list(h = h, use.directions = TRUE))
rrr =  inla(formula3,family="poisson",data=Germany,E=E,
           verbose = T, 
           control.inla = list(h = h, optimise.strategy = "smart"))
rrrr =  inla(formula3,family="poisson",data=Germany,E=E,
           verbose = T, 
           control.inla = list(h = h, optimise.strategy = "smart",
                               use.directions = TRUE))

print(round(dig = 4, r$misc$cov.intern))
print(round(dig = 4, rr$misc$cov.intern))
print(round(dig = 4, rrr$misc$cov.intern))
print(round(dig = 4, rrrr$misc$cov.intern))

print(c(r$cpu[2], rr$cpu[2], rrr$cpu[2], rrrr$cpu[2]))
print(c(r$misc$nfunc, rr$misc$nfunc,  rrr$misc$nfunc, rrrr$misc$nfunc))




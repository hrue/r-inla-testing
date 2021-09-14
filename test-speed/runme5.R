data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)
Germany <- rbind(Germany, Germany)
Germany <- rbind(Germany, Germany)
#Germany <- rbind(Germany, Germany)
#Germany <- rbind(Germany, Germany)
gr <- inla.read.graph(g)
nc <- 10
A <- matrix(rnorm(gr$n * nc), nc, gr$n)
e <- rep(0, nc)
Germany$rr <- sample(1:nc, dim(Germany)[1], replace = TRUE)
formula3 = Y ~ 1 +
    f(region.struct,model="besag", graph=g,
      scale.model = TRUE, replicate = rr) +
    ##extraconstr = list(A = A, e = e)) +
    f(region,model="iid") +
    f(x, model="rw2", scale.model = TRUE)
r = inla(formula3,family="poisson",data=Germany,E=E,
         verbose = TRUE,
         inla.call = "inla.mkl.work",
         num.threads = "6:1")
rr = inla(formula3,family="poisson",data=Germany,E=E,
         verbose = TRUE,
         inla.call = INLA:::inla.call.builtin(), 
         num.threads = "6:1")


r$mlik - rr$mlik
